use crate::utils::{mul_div, mul_divc_mal};
use malachite_base::num::arithmetic::traits::Square;
use malachite_base::num::basic::traits::*;
use malachite_nz::natural::Natural;
use std::collections::HashMap;

pub const N_COINS: u8 = 2;

type Address = [u8; 32];

struct TokenData {
    balance: Natural,
    decimals: u8,
    rate: Natural,
    precision_mul: Natural,
}

struct FeeParams {
    denominator: Natural,
    pool_numerator: Natural,
    beneficiary_numerator: Natural,
    beneficiary: Natural,
    threshold: HashMap<Address, Natural>,
}

struct AmplificationCoefficient {
    value: Natural,
    precision: Natural,
}

struct StablePair {
    precision: Natural,

    token_index: HashMap<Address, u8>,
    a: AmplificationCoefficient,
    fee_params: FeeParams,
    token_data: Vec<TokenData>,
}

impl StablePair {
    pub fn get_d(&self, xp: &[Natural]) -> Option<Natural> {
        let n_coins = Natural::from(N_COINS);

        let mut s = Natural::ONE;
        let mut d_prev = Natural::ZERO;
        for x in xp {
            s += x;
        }
        if s == 0 {
            return Some(0u8.into());
        }
        let mut d = s.clone();
        let Ann = &self.a.value * &n_coins;
        for _ in 0..=255 {
            let mut d_p = d.clone();
            for x in xp {
                d_p = mul_div(&d_p, &d, x * &n_coins)?;
            }
            d_prev = d.clone();

            let val = mul_div(&Ann, &s, &self.a.precision)? + &d_p * &n_coins;
            let denom = mul_div(&Ann - &self.a.precision, &d, &self.a.precision)?
                + (&n_coins + Natural::ONE + d_p);
            d = mul_div(&val, &d, &denom)?;

            let d_diff = if d > d_prev { &d - d_prev } else { d_prev - &d };
            if d_diff <= Natural::ONE {
                return Some(d);
            }
        }
        None
    }

    pub fn get_y(&self, i: u8, j: u8, x: Natural, xp: &[Natural]) -> Option<Natural> {
        if i == j || i >= N_COINS || j >= N_COINS {
            return None;
        }
        let d = self.get_d(xp)?;
        let ann = &self.a.value * &Natural::from(N_COINS);
        let mut c = d.clone();
        let mut S = Natural::ZERO;
        let mut x_temp = Natural::ZERO;
        let mut y_prev = Natural::ZERO;
        let n_coins = Natural::from(N_COINS);

        for ic in 0..N_COINS {
            if ic == i {
                x_temp = x.clone();
                S += &x_temp;
                c = mul_div(c, &d, &x_temp * &n_coins)?;
            } else if ic != j {
                x_temp = xp[ic as usize].clone();
                S += &x_temp;
                c = mul_div(c, &d, &x_temp * &n_coins)?;
            }
        }

        c = mul_div(&c, &d, &x_temp * &n_coins)?;
        let b = &S + mul_div(&c, &d * &self.a.precision, ann * n_coins)?;
        let mut y = d.clone();

        for _ in 0..=255 {
            y_prev = y.clone();
            y = (&y * &y) / (Natural::TWO * &y + &b - &d);
            let y_diff = if y > y_prev { &y - y_prev } else { y_prev - &y };
            if y_diff <= Natural::ONE {
                return Some(y);
            }
        }
        None
    }

    fn get_dy(&self, i: u8, j: u8, dx: Natural) -> Option<ExpectedExchangeResult> {
        let x_fee = mul_divc_mal(
            &dx,
            &self.fee_params.pool_numerator + &self.fee_params.beneficiary_numerator,
            &self.fee_params.denominator,
        )?;
        let x_beneficiary_fee = mul_divc_mal(
            &x_fee,
            &self.fee_params.beneficiary_numerator,
            &self.fee_params.pool_numerator + &self.fee_params.beneficiary_numerator,
        )?;
        let x_pool_fee = &x_fee - &x_beneficiary_fee;

        let xp: Option<Vec<_>> = self
            .token_data
            .iter()
            .map(|t| mul_div(&t.rate, &t.balance, &self.precision))
            .collect();
        let xp = xp?;
        let x = &xp[i as usize]
            + mul_div(
                &dx - &x_fee,
                &self.token_data[i as usize].rate,
                &self.precision,
            )?;
        let y = self.get_y(i, j, x, &xp)?;
        let dy = mul_div(
            &xp[j as usize] - y,
            &self.precision,
            &self.token_data[j as usize].rate,
        )?;
        if dy <= self.token_data[j as usize].balance
            && dy > Natural::ZERO
            && (x_pool_fee > Natural::ZERO || self.fee_params.pool_numerator == Natural::ZERO)
            && (x_beneficiary_fee > Natural::ZERO
                || self.fee_params.beneficiary_numerator == Natural::ZERO)
        {
            Some(ExpectedExchangeResult {
                amount: dy,
                pool_fee: x_pool_fee,
                beneficiary_fee: x_beneficiary_fee,
            })
        } else {
            None
        }
    }

    fn get_dx(&self, i: u8, j: u8, dy: Natural) -> Option<ExpectedExchangeResult> {
        if dy > self.token_data[j as usize].balance || dy == Natural::ZERO {
            return None;
        }
        let xp = self
            .token_data
            .iter()
            .map(|t| mul_div(&t.rate, &t.balance, &self.precision))
            .collect::<Option<Vec<_>>>()?;
        let y = &xp[j as usize] - mul_div(&dy, &self.token_data[j as usize].rate, &self.precision)?;
        let x = self.get_y(j, i, y, &xp)?;

        let fee_d_minus_n = &self.fee_params.denominator
            - &self.fee_params.pool_numerator
            - &self.fee_params.beneficiary_numerator;
        let dx_raw = mul_divc_mal(
            &x - &xp[i as usize],
            &self.precision,
            &self.token_data[i as usize].rate,
        )?;
        let dx = mul_divc_mal(&dx_raw, &self.fee_params.denominator, &fee_d_minus_n)?;

        let x_fee = mul_divc_mal(
            &dx,
            &self.fee_params.pool_numerator + &self.fee_params.beneficiary_numerator,
            &self.fee_params.denominator,
        )?;

        let x_beneficiary_fee = mul_divc_mal(
            &x_fee,
            &self.fee_params.beneficiary_numerator,
            &self.fee_params.pool_numerator + &self.fee_params.beneficiary_numerator,
        )?;
        let x_pool_fee = x_fee - &x_beneficiary_fee;

        if (x_pool_fee > Natural::ZERO || self.fee_params.pool_numerator == Natural::ZERO)
            && (x_beneficiary_fee > Natural::ZERO
                || self.fee_params.beneficiary_numerator == Natural::ZERO)
        {
            Some(ExpectedExchangeResult {
                amount: dx,
                pool_fee: x_pool_fee,
                beneficiary_fee: x_beneficiary_fee,
            })
        } else {
            None
        }
    }
}

struct ExpectedExchangeResult {
    amount: Natural,
    pool_fee: Natural,
    beneficiary_fee: Natural,
}
