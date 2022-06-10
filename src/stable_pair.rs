use std::collections::HashMap;

use malachite_base::num::{
    arithmetic::traits::Pow, basic::traits::*, conversion::traits::CheckedFrom,
};
use malachite_nz::natural::Natural;

use crate::{
    normal_pair::SwapResult,
    utils::{mul_div, mul_divc_mal},
};

pub const N_COINS: u8 = 2;

type Address = [u8; 32];

struct TokenData {
    balance: Natural,
    decimals: u8,
    rate: Natural,
    precision_mul: Natural,
}

pub struct FeeParams {
    pub denominator: Natural,
    pub pool_numerator: Natural,
    pub beneficiary_numerator: Natural,
}

pub struct AmplificationCoefficient {
    pub value: Natural,
    pub precision: Natural,
}

pub struct StablePair {
    precision: Natural,

    token_index: HashMap<Address, u8>,
    a: AmplificationCoefficient,
    fee: FeeParams,
    token_data: Vec<TokenData>,
}

pub struct TokenDataInput {
    pub decimals: u8,
    pub balance: u128,
}

impl StablePair {
    pub fn new(
        token_data: Vec<TokenDataInput>,
        token_index: HashMap<Address, u8>,
        a: AmplificationCoefficient,
        fee_params: FeeParams,
    ) -> Option<Self> {
        let max_token_decimals = token_data.iter().map(|td| td.decimals).max()?;
        let precision = Natural::from(10u8).pow(max_token_decimals as u64);
        let token_data = token_data
            .into_iter()
            .map(|td| {
                let decimals = td.decimals;
                let precision_mul = Natural::from(10u8).pow((max_token_decimals - decimals) as u64);
                let rate = &precision_mul * &precision;

                TokenData {
                    balance: Natural::from(td.balance),
                    decimals,
                    rate,
                    precision_mul,
                }
            })
            .collect::<Vec<_>>();

        Some(Self {
            precision,
            token_index,
            a,
            fee: fee_params,
            token_data,
        })
    }

    pub fn get_d(&self, xp: &[Natural]) -> Option<Natural> {
        let n_coins = Natural::from(N_COINS);

        let s: Natural = xp.iter().fold(Natural::ZERO, |acc, x| acc + x);
        let mut d_prev;

        if s == 0 {
            return Some(Natural::ZERO);
        }
        let mut d = s.clone();
        let ann = &self.a.value * &n_coins;

        for _ in 0..=255 {
            let mut d_p = d.clone();
            for x in xp {
                d_p = mul_div(&d_p, &d, x * &n_coins)?;
            }
            d_prev = d.clone();

            let val = mul_div(&ann, &s, &self.a.precision)? + &d_p * &n_coins;
            let denom = mul_div(&ann - &self.a.precision, &d, &self.a.precision)?
                + (&n_coins + Natural::ONE) * &d_p;
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

        let n_coins = Natural::from(N_COINS);

        let d = self.get_d(xp)?;
        let ann = &self.a.value * &n_coins;
        let mut c = d.clone();
        let mut s = Natural::ZERO;
        let mut x_temp;
        let mut y_prev;

        for ic in 0..N_COINS {
            if ic == i {
                x_temp = x.clone();
                s += &x_temp;
                c = mul_div(c, &d, &x_temp * &n_coins)?;
            } else if ic != j {
                x_temp = xp[ic as usize].clone();
                s += &x_temp;
                c = mul_div(c, &d, &x_temp * &n_coins)?;
            }
        }

        c = mul_div(&c, &d * &self.a.precision, &ann * &n_coins)?;
        let b = &s + mul_div(&d, &self.a.precision, ann)?;
        let mut y = d.clone();

        for _ in 0..=255 {
            y_prev = y.clone();
            y = (&y * &y + &c) / (Natural::TWO * &y + &b - &d);
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
            &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
            &self.fee.denominator,
        )?;
        let x_beneficiary_fee = mul_div(
            &x_fee,
            &self.fee.beneficiary_numerator,
            &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
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
            && (x_pool_fee > Natural::ZERO || self.fee.pool_numerator == Natural::ZERO)
            && (x_beneficiary_fee > Natural::ZERO
                || self.fee.beneficiary_numerator == Natural::ZERO)
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

        let fee_d_minus_n =
            &self.fee.denominator - &self.fee.pool_numerator - &self.fee.beneficiary_numerator;
        let dx_raw = mul_divc_mal(
            &x - &xp[i as usize],
            &self.precision,
            &self.token_data[i as usize].rate,
        )?;
        let dx = mul_divc_mal(&dx_raw, &self.fee.denominator, &fee_d_minus_n)?;

        let x_fee = mul_divc_mal(
            &dx,
            &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
            &self.fee.denominator,
        )?;

        let x_beneficiary_fee = mul_divc_mal(
            &x_fee,
            &self.fee.beneficiary_numerator,
            &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
        )?;
        let x_pool_fee = x_fee - &x_beneficiary_fee;

        if (x_pool_fee > Natural::ZERO || self.fee.pool_numerator == Natural::ZERO)
            && (x_beneficiary_fee > Natural::ZERO
                || self.fee.beneficiary_numerator == Natural::ZERO)
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

    pub fn expected_exchange(&self, amount: u128, spent_token: &Address) -> Option<SwapResult> {
        let i = *self.token_index.get(spent_token)?;
        let j = if i == 0 { 1 } else { 0 };
        let result = self
            .get_dy(i, j, Natural::from(amount))
            .unwrap_or(ExpectedExchangeResult {
                amount: Natural::ZERO,
                pool_fee: Natural::ZERO,
                beneficiary_fee: Natural::ZERO,
            });

        Some(SwapResult {
            amount: u128::checked_from(&result.amount)?,
            fee: u128::checked_from(&(result.pool_fee + result.beneficiary_fee))?,
        })
    }

    pub fn expected_spend_amount(
        &self,
        receive_amount: u128,
        receive_token_root: &Address,
    ) -> Option<SwapResult> {
        let j = *self.token_index.get(receive_token_root)?;
        let i = if j == 0 { 1 } else { 0 };
        let result =
            self.get_dx(i, j, Natural::from(receive_amount))
                .unwrap_or(ExpectedExchangeResult {
                    amount: Natural::ZERO,
                    pool_fee: Natural::ZERO,
                    beneficiary_fee: Natural::ZERO,
                });

        Some(SwapResult {
            amount: u128::checked_from(&result.amount)?,
            fee: u128::checked_from(&(result.pool_fee + result.beneficiary_fee))?,
        })
    }
}

#[derive(Debug, Clone)]
struct ExpectedExchangeResult {
    amount: Natural,
    pool_fee: Natural,
    beneficiary_fee: Natural,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expected_exchange() {
        let pair = create_pair();

        let res = pair.expected_exchange(12, &[0; 32]).unwrap();
        assert_eq!(res.amount, 10999632243);
        assert_eq!(res.fee, 1);

        let res = pair.expected_exchange(12, &[1; 32]).unwrap();
        assert_eq!(res.amount, 0);
        assert_eq!(res.fee, 0);

        let res = pair.expected_exchange(1122131234, &[1; 32]).unwrap();
        assert_eq!(res.amount, 1);
        assert_eq!(res.fee, 3366394);

        let res = pair
            .expected_exchange(170141183460469231731687303715884105728, &[1; 32])
            .unwrap();
        assert_eq!(res.amount, 5530869000000000);
        assert_eq!(res.fee, 510423550381407695195061911147652318);
    }

    #[test]
    fn test_expected_spend_amount() {
        let pair = create_pair();

        let res = pair.expected_spend_amount(1, &[0; 32]).unwrap();
        assert_eq!(res.amount, 1002975494);
        assert_eq!(res.fee, 3008927);

        let res = pair.expected_spend_amount(12, &[0; 32]).unwrap();
        assert_eq!(res.amount, 12035705928);
        assert_eq!(res.fee, 36107118);

        let res = pair
            .expected_spend_amount(17014118346012346, &[0; 32])
            .unwrap();
        assert_eq!(res.amount, 0);
        assert_eq!(res.fee, 0);

        let res = pair
            .expected_spend_amount(17014118346012346, &[1; 32])
            .unwrap();
        assert_eq!(res.amount, 17065886);
        assert_eq!(res.fee, 51198);
    }

    fn create_pair() -> StablePair {
        let token_data = vec![
            TokenDataInput {
                decimals: 9,
                balance: 5530869000000000,
            },
            TokenDataInput {
                decimals: 18,
                balance: 5514989303312229845534954,
            },
        ];

        let token_index = HashMap::from([([0; 32], 0), ([1; 32], 1)]);
        let pair = StablePair::new(
            token_data,
            token_index,
            AmplificationCoefficient {
                value: Natural::from(85u32),
                precision: Natural::ONE,
            },
            FeeParams {
                denominator: Natural::from(1000000u32),
                pool_numerator: Natural::from(3000u64),
                beneficiary_numerator: Natural::ZERO,
                // beneficiary: Default::default(),
                // threshold: Default::default(),
            },
        )
        .unwrap();
        pair
    }
}
