use anyhow::anyhow;
use malachite_base::num::arithmetic::traits::DivRound;
use malachite_base::num::{arithmetic::traits::Pow, basic::traits::*};
use malachite_base::rounding_modes::RoundingMode;
use malachite_nz::natural::Natural;
use std::borrow::Borrow;
use std::collections::HashMap;

use crate::normal_pair::OneAmountSwapResult;
use crate::{
    normal_pair::SwapResult,
    utils::{mul_div, mul_divc_mal},
};

type Address = [u8; 32];

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct TokenData {
    balance: Natural,
    decimals: u8,
    rate: Natural,
    precision_mul: Natural,
}

#[derive(Clone, Debug)]
pub struct FeeParams {
    pub denominator: Natural,
    pub pool_numerator: Natural,
    pub beneficiary_numerator: Natural,
}

#[derive(Clone, Debug)]
pub struct AmplificationCoefficient {
    pub value: Natural,
    pub precision: Natural,
}

#[derive(Clone, Debug)]
pub struct StablePair {
    precision: Natural,

    token_index: HashMap<Address, u8>,
    a: AmplificationCoefficient,
    fee: FeeParams,
    token_data: Vec<TokenData>,
    lp_supply: Natural,
}

pub struct TokenDataInput {
    pub decimals: u8,
    pub balance: u128,
}

fn try_from_token_data_input_to_token_data_and_precision(
    token_data: Vec<TokenDataInput>,
) -> Option<(Vec<TokenData>, Natural)> {
    let max_token_decimals = token_data.iter().map(|td| td.decimals).max()?;
    let precision = Natural::from(10u8).pow(max_token_decimals as u64);
    Some((
        token_data
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
            .collect::<Vec<_>>(),
        precision,
    ))
}

impl StablePair {
    pub fn new(
        token_data: Vec<TokenDataInput>,
        token_index: HashMap<Address, u8>,
        a: AmplificationCoefficient,
        fee_params: FeeParams,
        lp_supply: u128,
    ) -> Option<Self> {
        let (token_data, precision) =
            try_from_token_data_input_to_token_data_and_precision(token_data)?;

        Some(Self {
            precision,
            token_index,
            a,
            fee: fee_params,
            token_data,
            lp_supply: Natural::from(lp_supply),
        })
    }

    pub fn get_d(&self, xp: &[Natural]) -> Option<Natural> {
        let n_coins = Natural::from(xp.len());

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
        let n_coins_small = xp.len() as u8;

        if i == j || i >= n_coins_small || j >= n_coins_small {
            return None;
        }

        let n_coins = Natural::from(n_coins_small);
        let d = self.get_d(xp)?;
        let ann = &self.a.value * &n_coins;
        let mut c = d.clone();
        let mut s = Natural::ZERO;
        let mut x_temp;
        let mut y_prev;

        for ic in 0..n_coins_small {
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
        let dx = mul_divc_mal(dx_raw, &self.fee.denominator, fee_d_minus_n)?;

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
        let j = u8::from(i == 0);
        let result = self
            .get_dy(i, j, Natural::from(amount))
            .unwrap_or(ExpectedExchangeResult {
                amount: Natural::ZERO,
                pool_fee: Natural::ZERO,
                beneficiary_fee: Natural::ZERO,
            });

        Some(SwapResult {
            amount: u128::try_from(&result.amount).ok()?,
            fee: u128::try_from(&(result.pool_fee + result.beneficiary_fee)).ok()?,
        })
    }

    pub fn expected_exchange_extended(
        &self,
        amount: u128,
        spent_token: &Address,
        receive_token: &Address,
    ) -> Option<SwapResult> {
        let i = *self.token_index.get(spent_token)?;
        let j = *self.token_index.get(receive_token)?;

        let result = self
            .get_dy(i, j, Natural::from(amount))
            .unwrap_or(ExpectedExchangeResult {
                amount: Natural::ZERO,
                pool_fee: Natural::ZERO,
                beneficiary_fee: Natural::ZERO,
            });

        Some(SwapResult {
            amount: u128::try_from(&result.amount).ok()?,
            fee: u128::try_from(&(result.pool_fee + result.beneficiary_fee)).ok()?,
        })
    }

    pub fn expected_exchange_extended_one_amount(
        &self,
        spent_token: &Address,
        receive_token: &Address,
    ) -> Option<OneAmountSwapResult> {
        let i = *self.token_index.get(spent_token)?;
        let j = *self.token_index.get(receive_token)?;

        let i_token_data = self.token_data.get(i as usize)?;
        let j_token_data = self.token_data.get(j as usize)?;

        let one_amount = 10_u128.pow(i_token_data.decimals as u32);

        let result =
            self.get_dy(i, j, Natural::from(one_amount))
                .unwrap_or(ExpectedExchangeResult {
                    amount: Natural::ZERO,
                    pool_fee: Natural::ZERO,
                    beneficiary_fee: Natural::ZERO,
                });

        Some(OneAmountSwapResult {
            amount: u128::try_from(&result.amount).ok()?,
            fee: u128::try_from(&(result.pool_fee + result.beneficiary_fee)).ok()?,
            decimals: j_token_data.decimals,
        })
    }

    pub fn expected_spend_amount(
        &self,
        receive_amount: u128,
        receive_token_root: &Address,
    ) -> Option<SwapResult> {
        let j = *self.token_index.get(receive_token_root)?;
        let i = u8::from(j == 0);
        let result =
            self.get_dx(i, j, Natural::from(receive_amount))
                .unwrap_or(ExpectedExchangeResult {
                    amount: Natural::ZERO,
                    pool_fee: Natural::ZERO,
                    beneficiary_fee: Natural::ZERO,
                });

        Some(SwapResult {
            amount: u128::try_from(&result.amount).ok()?,
            fee: u128::try_from(&(result.pool_fee + result.beneficiary_fee)).ok()?,
        })
    }

    pub fn expected_spend_amount_extended(
        &self,
        receive_amount: u128,
        receive_token_root: &Address,
        spent_token_root: &Address,
    ) -> Option<SwapResult> {
        let j = *self.token_index.get(receive_token_root)?;
        let i = *self.token_index.get(spent_token_root)?;

        let result =
            self.get_dx(i, j, Natural::from(receive_amount))
                .unwrap_or(ExpectedExchangeResult {
                    amount: Natural::ZERO,
                    pool_fee: Natural::ZERO,
                    beneficiary_fee: Natural::ZERO,
                });

        Some(SwapResult {
            amount: u128::try_from(&result.amount).ok()?,
            fee: u128::try_from(&(result.pool_fee + result.beneficiary_fee)).ok()?,
        })
    }

    pub fn expected_spend_amount_extended_one_amount(
        &self,
        receive_token_root: &Address,
        spent_token_root: &Address,
    ) -> Option<OneAmountSwapResult> {
        let j = *self.token_index.get(receive_token_root)?;
        let i = *self.token_index.get(spent_token_root)?;

        let j_token_data = self.token_data.get(j as usize)?;
        let i_token_data = self.token_data.get(i as usize)?;

        let one_amount = 10_u128.pow(j_token_data.decimals as u32);

        let result =
            self.get_dx(i, j, Natural::from(one_amount))
                .unwrap_or(ExpectedExchangeResult {
                    amount: Natural::ZERO,
                    pool_fee: Natural::ZERO,
                    beneficiary_fee: Natural::ZERO,
                });

        Some(OneAmountSwapResult {
            amount: u128::try_from(&result.amount).ok()?,
            fee: u128::try_from(&(result.pool_fee + result.beneficiary_fee)).ok()?,
            decimals: i_token_data.decimals,
        })
    }

    pub fn update_balances(
        &mut self,
        balances: Vec<u128>,
        lp_supply: u128,
    ) -> Result<(), anyhow::Error> {
        for (i, balance) in balances.into_iter().enumerate() {
            let token = self
                .token_data
                .get_mut(i)
                .ok_or_else(|| anyhow!("invalid tokens len"))?;
            token.balance = balance.into();
        }
        self.lp_supply = lp_supply.into();
        Ok(())
    }

    pub fn update_balances_and_decimals(
        &mut self,
        balances: Vec<TokenDataInput>,
    ) -> Result<(), anyhow::Error> {
        let (token_data, _) = try_from_token_data_input_to_token_data_and_precision(balances)
            .ok_or_else(|| anyhow!("cant to token data: cant get max_token_decimals"))?;

        for (index, token_data) in token_data.into_iter().enumerate() {
            let token = self
                .token_data
                .get_mut(index)
                .ok_or_else(|| anyhow!("invalid tokens len"))?;
            *token = token_data;
        }

        Ok(())
    }

    pub fn update_fee_params(&mut self, fee_params: FeeParams) {
        self.fee = fee_params;
    }

    fn xp_mem<I, Item>(&self, balances: I) -> Option<Vec<Natural>>
    where
        I: Iterator<Item = Item>,
        Item: Borrow<Natural>,
    {
        let mut result = Vec::new();
        for (i, balance) in balances.into_iter().enumerate() {
            let token_data = self.token_data.get(i)?;
            result.push(mul_div(&token_data.rate, balance, &self.precision)?);
        }

        Some(result)
    }

    pub fn expected_deposit_liquidity(&self, amounts: &[u128]) -> Option<DepositLiquidityResultV2> {
        let old_balances: Vec<_> = self
            .token_data
            .iter()
            .map(|x| &x.balance)
            .cloned()
            .collect();

        let d0 = self.get_d(&self.xp_mem(old_balances.iter())?)?;

        let mut new_balances = old_balances.clone();
        let mut pool_fees = vec![Natural::ZERO; self.token_data.len()];
        let mut beneficiary_fees = vec![Natural::ZERO; self.token_data.len()];
        let mut result_balances = old_balances.clone();
        let mut differences = vec![Natural::ZERO; self.token_data.len()];
        let lp_reward;
        let mut sell = vec![false; self.token_data.len()];

        let has_zero_balance = amounts.iter().any(|x| *x == 0);
        for (i, amount) in amounts.iter().enumerate() {
            new_balances[i] += Natural::from(*amount);
        }

        let d1 = self.get_d(&self.xp_mem(new_balances.iter())?)?;
        if has_zero_balance && self.lp_supply == Natural::ZERO || d0 >= d1 {
            return None;
        }

        let num_coins = self.token_data.len() as u128;

        if self.lp_supply > Natural::ZERO {
            let fee_numerator = mul_div(
                &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
                Natural::from(num_coins),
                Natural::from(4 * (num_coins - 1)),
            )?;

            for i in 0..self.token_data.len() {
                let ideal_balance = mul_div(&d1, &old_balances[i], &d0)?;
                let new_balance = &new_balances[i];
                let difference = if &ideal_balance > new_balance {
                    &ideal_balance - new_balance
                } else {
                    new_balance - &ideal_balance
                };
                sell[i] = &ideal_balance < new_balance;
                let fees = mul_divc_mal(&fee_numerator, &difference, &self.fee.denominator)?;
                beneficiary_fees[i] = mul_div(
                    &fees,
                    &self.fee.beneficiary_numerator,
                    &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
                )?;
                pool_fees[i] = &fees - &beneficiary_fees[i];
                result_balances[i] = new_balance - &beneficiary_fees[i];
                new_balances[i] = &new_balances[i] - &pool_fees[i] - &beneficiary_fees[i];
                differences[i] = difference.clone();
            }

            let d2 = self.get_d(&self.xp_mem(new_balances.iter())?)?;
            lp_reward = (&mul_div(&self.lp_supply, d2 - &d0, &d0)?)
                .try_into()
                .ok()?;
        } else {
            result_balances = new_balances;
            lp_reward = u128::try_from(&d1).ok()?;
        };

        Some(DepositLiquidityResultV2 {
            old_balances: old_balances
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            amounts: amounts.to_vec(),
            lp_reward,
            result_balances: result_balances
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            invariant: (&d1).try_into().ok()?,
            differences: differences
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            sell,
            pool_fees: pool_fees
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            beneficiary_fees: beneficiary_fees
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
        })
    }

    pub fn expected_withdraw(&self, lp_amount: u128) -> Option<(u128, u128)> {
        let left_back_amount = mul_div(
            &self.token_data[0].balance,
            Natural::from(lp_amount),
            &self.lp_supply,
        )?;
        let right_back_amount = mul_div(
            &self.token_data[1].balance,
            Natural::from(lp_amount),
            &self.lp_supply,
        )?;

        Some((
            (&left_back_amount).try_into().ok()?,
            (&right_back_amount).try_into().ok()?,
        ))
    }

    pub fn expected_witdraw_one_coin(&self, lp_amount: u128, address: [u8; 32]) -> Option<u128> {
        let index = *self.token_index.get(&address)?;

        self.get_expected_lp_amount(index as usize, lp_amount)
    }

    pub fn get_expected_lp_amount(&self, i: usize, dy: u128) -> Option<u128> {
        let token_data = &self.token_data.get(i)?;
        let dy = Natural::from(dy);
        if dy >= token_data.balance || dy == 0 {
            return None;
        }

        let mut xp = Vec::new();
        for token_data in self.token_data.iter() {
            let rate = &token_data.rate;
            let balance = &token_data.balance;
            let xp_i = mul_div(rate, balance, &self.precision)?;
            xp.push(xp_i);
        }

        let d0 = self.get_d(&xp)?;
        let dy = mul_divc_mal(&dy, &token_data.rate, &self.precision)?;
        xp[i] = &xp[i] - &dy;
        let d1 = self.get_d(&xp)?;

        let lp_raw = mul_divc_mal(&self.lp_supply, &d0 - &d1, &d0)?;
        let lp_res = mul_divc_mal(
            lp_raw,
            &self.fee.denominator,
            &self.fee.denominator - (&self.fee.pool_numerator + &self.fee.beneficiary_numerator),
        )?;

        u128::try_from(&lp_res).ok()
    }

    // expectedOneCoinWithdrawalSpendAmount
    pub fn expected_one_coin_withdrawal_spend_amount(
        &self,
        receive_amount: u128,
        receive_token_root: [u8; 32],
    ) -> Option<u128> {
        let i = *self.token_index.get(&receive_token_root)?;

        self.get_expected_lp_amount(i as usize, receive_amount)
    }

    // expectedWithdrawLiquidityOneCoin
    pub fn expected_withdraw_liquidity_one_coin(
        &self,
        amount: u128,
        token: [u8; 32],
    ) -> Option<WithdrawResultV2> {
        let index = self.token_index.get(&token)?;
        self.expected_withdraw_liquidity_one_coin_inner(amount, *index as usize)
    }

    #[allow(non_snake_case)]
    fn expected_withdraw_liquidity_one_coin_inner(
        &self,
        token_amount: u128,
        i: usize,
    ) -> Option<WithdrawResultV2> {
        let old_balances: Vec<_> = self.token_data.iter().map(|x| x.balance.clone()).collect();
        let old_lp_supply = self.lp_supply.clone();

        let token_amount = Natural::from(token_amount);

        let mut pool_fees = vec![Natural::ZERO; self.token_data.len()];
        let mut beneficiary_fees = vec![Natural::ZERO; self.token_data.len()];
        let mut differences = vec![0u128; self.token_data.len()];
        let mut sell = vec![false; self.token_data.len()];
        let mut amounts = vec![Natural::ZERO; self.token_data.len()];

        let xp_mem = self.xp_mem(old_balances.iter())?;

        let mut result_balances = old_balances.clone();

        let D0 = self.get_d(&xp_mem)?;
        let D1 = &D0 - &mul_div(&token_amount, &D0, &old_lp_supply)?;
        let lp_fee = mul_divc_mal(
            &token_amount,
            &self.fee.beneficiary_numerator + &self.fee.pool_numerator,
            &self.fee.denominator,
        )?;
        let d1_fee = &D0 - &mul_div(&token_amount - &lp_fee, &D0, &old_lp_supply)?;

        let new_y0 = self.get_y_d(i, xp_mem.clone(), D1.clone())?;
        let new_y = self.get_y_d(i, xp_mem.clone(), d1_fee)?;

        let dy_0 = (&xp_mem[i] - new_y0) / &self.token_data[i].precision_mul; // without fee

        let dy = (&xp_mem[i] - &new_y) / &self.token_data[i].precision_mul;

        for j in 0..self.token_data.len() {
            let dx_expected = if j == i {
                sell[j] = false;
                mul_div(&xp_mem[j], &D1, &D0)? - &new_y
            } else {
                sell[j] = true;
                &xp_mem[j] - mul_div(&xp_mem[j], &D1, &D0)?
            };
            differences[j] = (&dx_expected).try_into().ok()?;
        }

        let dy_fee = &dy_0 - &dy;
        beneficiary_fees[i] = mul_div(
            &dy_fee,
            &self.fee.beneficiary_numerator,
            &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
        )?;
        pool_fees[i] = &dy_fee - &beneficiary_fees[i];
        amounts[i] = dy.clone();
        result_balances[i] = &old_balances[i] - dy;

        Some(WithdrawResultV2 {
            lp_amount: (&token_amount).try_into().ok()?,
            old_balances: old_balances
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            amounts: amounts
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            result_balances: result_balances
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            invariant: (&D1).try_into().ok()?,
            differences,
            sell,
            pool_fees: pool_fees
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            beneficiary_fees: beneficiary_fees
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
        })
    }

    #[allow(non_snake_case)]
    fn get_y_d(&self, i: usize, xp: Vec<Natural>, D: Natural) -> Option<Natural> {
        if i >= self.token_data.len() {
            return None;
        }

        let n_coins = Natural::from(self.token_data.len());

        let Ann = &self.a.value * Natural::from(self.token_data.len());
        let mut c = D.clone();
        let mut S = Natural::ZERO;
        let mut _x = Natural::ZERO;

        for _i in 0..self.token_data.len() {
            if _i == i {
                continue;
            }
            _x = xp[_i].clone();
            S += &_x;
            c = mul_div(&c, &D, &_x * &n_coins)?;
        }

        c = mul_div(&c, &D * &self.a.precision, &Ann * &n_coins)?;
        let b = &S + mul_div(&D, &self.a.precision, &Ann)?;

        let mut y_prev;
        let mut y = D.clone();

        for _i in 0..=255 {
            y_prev = y.clone();
            y = (&y * &y + &c) / (&y * Natural::from(2u8) + &b - &D);
            let diff = if y > y_prev { &y - y_prev } else { y_prev - &y };
            if diff <= Natural::ONE {
                return Some(y);
            }
        }

        None
    }

    pub fn expected_one_coin_deposit_liquidity(
        &self,
        spent_token_root: Address,
        amount: u128,
    ) -> Option<DepositLiquidityResultV2> {
        let i = self.token_index.get(&spent_token_root)?;
        self.expected_one_coin_deposit_liquidity_inner(Natural::from(amount), *i as usize)
    }

    fn expected_one_coin_deposit_liquidity_inner(
        &self,
        amount: Natural,
        i: usize,
    ) -> Option<DepositLiquidityResultV2> {
        let old_balances = self
            .token_data
            .iter()
            .map(|x| x.balance.clone())
            .collect::<Vec<_>>();

        let d0 = self.get_d(&self.xp_mem(old_balances.iter())?)?;

        let mut new_balances = old_balances.clone();

        let mut pool_fees = vec![Natural::ZERO; self.token_data.len()];
        let mut beneficiary_fees = vec![Natural::ZERO; self.token_data.len()];
        let mut result_balances = old_balances.clone();
        let mut differences = vec![Natural::ZERO; self.token_data.len()];
        let mut amounts = vec![Natural::ZERO; self.token_data.len()];

        let mut sell = vec![false; self.token_data.len()];

        new_balances[i] += &amount;
        amounts[i] = amount.clone();

        let d1 = self.get_d(&self.xp_mem(new_balances.iter())?)?;

        if d0 >= d1 {
            return None;
        }

        for j in 0..self.token_data.len() {
            let ideal_balance = mul_div(&d1, &old_balances[j], &d0)?;
            let new_balance = new_balances[j].clone();
            let difference = if ideal_balance > new_balance {
                &ideal_balance - &new_balance
            } else {
                &new_balance - &ideal_balance
            };
            differences[j] = difference.clone();
            sell[j] = ideal_balance < new_balance;
        }

        let fees = mul_div(
            &amount,
            &self.fee.beneficiary_numerator + &self.fee.pool_numerator,
            &self.fee.denominator,
        )?;
        beneficiary_fees[i] = mul_div(
            &fees,
            &self.fee.beneficiary_numerator,
            &self.fee.pool_numerator + &self.fee.beneficiary_numerator,
        )?;
        pool_fees[i] = &fees - &beneficiary_fees[i];
        result_balances[i] = &new_balances[i] - &beneficiary_fees[i];
        new_balances[i] = &new_balances[i] - &pool_fees[i] - &beneficiary_fees[i];

        let d2 = self.get_d(&self.xp_mem(new_balances.iter())?)?;
        let lp_reward = mul_div(&self.lp_supply, &d2 - &d0, &d0)?;

        if lp_reward == Natural::ZERO {
            return None;
        }

        Some(DepositLiquidityResultV2 {
            old_balances: old_balances
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            amounts: amounts
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            lp_reward: (&lp_reward).try_into().ok()?,
            result_balances: result_balances
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            invariant: (&d1).try_into().ok()?,
            differences: differences
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            sell,
            pool_fees: pool_fees
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
            beneficiary_fees: beneficiary_fees
                .iter()
                .map(|x| x.try_into().ok())
                .collect::<Option<Vec<_>>>()?,
        })
    }

    pub fn expected_deposit_spend_amount(&self, root: Address, amount: u128) -> Option<u128> {
        let i = self.token_index.get(&root)?;

        self.expected_deposit_spend_amount_inner(*i as usize, &Natural::from(amount))
    }

    fn expected_deposit_spend_amount_inner(&self, i: usize, lp: &Natural) -> Option<u128> {
        if lp > &self.lp_supply || lp == &Natural::ZERO {
            return None;
        }

        let xp = self.xp_mem(self.token_data.iter().map(|x| x.balance.clone()))?;
        let d0 = self.get_d(&xp)?;
        let d2 = if self.lp_supply > Natural::ZERO {
            &d0 + mul_divc_mal(&d0, lp, &self.lp_supply)?
        } else {
            lp.clone()
        };

        let y_minus_fee = self.get_y_d(i, xp.clone(), d2)?;
        let dy_minus_fee = &y_minus_fee - &xp[i];
        let dy = mul_divc_mal(
            dy_minus_fee,
            &self.fee.denominator,
            &self.fee.denominator - (&self.fee.beneficiary_numerator + &self.fee.pool_numerator),
        )?;

        (&dy.div_round(
            self.token_data[i].precision_mul.clone(),
            RoundingMode::Ceiling,
        ))
            .try_into()
            .ok()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WithdrawResultV2 {
    pub lp_amount: u128,
    pub old_balances: Vec<u128>,
    pub amounts: Vec<u128>,
    pub result_balances: Vec<u128>,
    pub invariant: u128,
    pub differences: Vec<u128>,
    pub sell: Vec<bool>,
    pub pool_fees: Vec<u128>,
    pub beneficiary_fees: Vec<u128>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DepositLiquidityResultV2 {
    pub old_balances: Vec<u128>,
    pub amounts: Vec<u128>,
    pub lp_reward: u128,
    pub result_balances: Vec<u128>,
    pub invariant: u128,
    pub differences: Vec<u128>,
    pub sell: Vec<bool>,
    pub pool_fees: Vec<u128>,
    pub beneficiary_fees: Vec<u128>,
}

#[derive(Debug, Clone)]
struct ExpectedExchangeResult {
    amount: Natural,
    pool_fee: Natural,
    beneficiary_fee: Natural,
}

#[cfg(test)]
mod tests {
    use eint::Eint;

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

        let token_data = vec![
            TokenDataInput {
                decimals: 18,
                balance: 899998992992000563087962,
            },
            TokenDataInput {
                decimals: 6,
                balance: 900003012011,
            },
            TokenDataInput {
                decimals: 6,
                balance: 899997995000,
            },
        ];

        let token_index = HashMap::from([([0; 32], 0), ([1; 32], 1), ([2; 32], 2)]);
        let pair = StablePair::new(
            token_data,
            token_index,
            AmplificationCoefficient {
                value: Natural::from(2000u32),
                precision: Natural::ONE,
            },
            FeeParams {
                denominator: Natural::from(1000000u32),
                pool_numerator: Natural::from(250u64),
                beneficiary_numerator: Natural::from(250u64),
            },
            2700000000000000,
        )
        .unwrap();

        let res = pair
            .expected_exchange_extended(1000, &[1; 32], &[0; 32])
            .unwrap();
        assert_eq!(res.amount, 0);
        assert_eq!(res.fee, 0);

        let token_data = vec![
            TokenDataInput {
                decimals: 6,
                balance: 11190776580,
            },
            TokenDataInput {
                decimals: 18,
                balance: 10760701702466899679822,
            },
            TokenDataInput {
                decimals: 6,
                balance: 11149517599,
            },
        ];

        let token_index = HashMap::from([([0; 32], 0), ([1; 32], 1), ([2; 32], 2)]);
        let pair = StablePair::new(
            token_data,
            token_index,
            AmplificationCoefficient {
                value: Natural::from(2000u32),
                precision: Natural::ONE,
            },
            FeeParams {
                denominator: Natural::from(1000000u32),
                pool_numerator: Natural::from(0u64),
                beneficiary_numerator: Natural::from(500u64),
            },
            33100993305275,
        )
        .unwrap();

        let res = pair
            .expected_exchange_extended(1000, &[0; 32], &[1; 32])
            .unwrap();
        assert_eq!(res.amount, 998980317545819);
        assert_eq!(res.fee, 1);
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

    #[test]
    fn test_expected_deposit() {
        let pair = create_withdraw_target();

        let res = pair.expected_deposit_liquidity(&[1000, 100]).unwrap();

        assert_eq!(
            res.old_balances,
            vec![1515096645740791, 9940268571486323793074983]
        );
        assert_eq!(res.amounts, vec![1000, 100]);
        assert_eq!(res.lp_reward, 512727596511);
        assert_eq!(
            res.result_balances,
            vec![1515096645741790, 9940268571486323569697543]
        );
        assert_eq!(res.invariant, 11422079333653762020989407);
        assert_eq!(res.differences, vec![864, 893510158421]);
        assert_eq!(res.sell, vec![true, false]);
        assert_eq!(res.pool_fees, vec![0, 0]);
        assert_eq!(res.beneficiary_fees, vec![1, 223377540]);

        dbg!(res);

        let res = pair.expected_deposit_liquidity(&[100000, 1000000]).unwrap();
        assert_eq!(
            res.old_balances,
            vec![1515096645740791, 9940268571486323793074983]
        );
        assert_eq!(res.amounts, vec![100000, 1000000]);
        assert_eq!(res.lp_reward, 51312801583529);
        assert_eq!(
            res.result_balances,
            vec![1515096645840769, 9940268571486301456321056]
        );
        assert_eq!(res.invariant, 11422079333755406020866269);
        assert_eq!(res.differences, vec![86382, 89351015707201]);
        assert_eq!(res.sell, vec![true, false]);
        assert_eq!(res.pool_fees, vec![0, 0]);
        assert_eq!(res.beneficiary_fees, vec![22, 22337753927]);
    }

    #[test]
    fn expected_withdraw() {
        let pair = create_withdraw_target();
        let (left, right) = pair.expected_withdraw(1488).unwrap();
        assert_eq!(left, 0);
        assert_eq!(right, 2589);

        let (left, right) = pair.expected_withdraw(438348983948).unwrap();
        assert_eq!(left, 116);
        assert_eq!(right, 762964625778);

        let (left, right) = pair.expected_withdraw(0).unwrap();
        assert_eq!(left, 0);
        assert_eq!(right, 0);

        let (left, right) = pair.expected_withdraw(1).unwrap();
        assert_eq!(left, 0);
        assert_eq!(right, 1);

        let (left, right) = pair.expected_withdraw(2).unwrap();
        assert_eq!(left, 0);
        assert_eq!(right, 3);

        let pair = npool_for_withdraws();

        let one: [u8; 32] =
            hex::decode("836426076347dc312b950be54846d6415795b32bc664a08ceb034fd7358a2388")
                .unwrap()
                .try_into()
                .unwrap();

        let res = pair.expected_witdraw_one_coin(100, one).unwrap();
        assert_eq!(res, 101);
        let res = pair.expected_witdraw_one_coin(1001, one).unwrap();
        assert_eq!(res, 1003);

        let two = hex::decode("e35765673ccc6c11889483944fcbc005af10c5980eada57f7f080442252f2e2a")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair.expected_witdraw_one_coin(10001, two).unwrap();
        assert_eq!(res, 10032);
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
            },
            5711020512957239363328239,
        )
        .unwrap();
        pair
    }

    #[test]
    fn test_real() {
        let pair = create_real();

        let res = pair.expected_exchange(1_000_000, &[0; 32]).unwrap();
        dbg!(res);
    }

    fn create_real() -> StablePair {
        let token_data = vec![
            TokenDataInput {
                decimals: 18,
                balance: 4988679616589 * 1_000_000,
            },
            TokenDataInput {
                decimals: 18,
                balance: 4989613045598 * 1_000_000,
            },
        ];

        let token_index = HashMap::from([([0; 32], 0), ([1; 32], 1)]);
        let pair = StablePair::new(
            token_data,
            token_index,
            AmplificationCoefficient {
                value: Natural::from(200u32),
                precision: Natural::ONE,
            },
            FeeParams {
                denominator: Natural::from(1000000u32),
                pool_numerator: Natural::from(0u64),
                beneficiary_numerator: Natural::from(500u64),
            },
            5711020512957239363328239,
        )
        .unwrap();
        pair
    }

    fn create_withdraw_target() -> StablePair {
        let token_data = vec![
            TokenDataInput {
                decimals: 9,
                balance: 1515096645740791,
            },
            TokenDataInput {
                decimals: 18,
                balance: 9940268571486323793074983,
            },
        ];

        let token_index = HashMap::from([([0; 32], 0), ([1; 32], 1)]);
        let pair = StablePair::new(
            token_data,
            token_index,
            AmplificationCoefficient {
                value: Natural::from(200u32),
                precision: Natural::ONE,
            },
            FeeParams {
                denominator: Natural::from(1000000u32),
                pool_numerator: Natural::from(0u64),
                beneficiary_numerator: Natural::from(500u64),
            },
            5711020512957239363328239,
        )
        .unwrap();
        pair
    }

    #[test]
    fn test_npool() {
        let one: [u8; 32] =
            hex::decode("0b23ee4983cd1fa2372cf10abf04100414baa9dfd78cf79dcd06c90b60439fb8")
                .unwrap()
                .try_into()
                .unwrap();
        let two: [u8; 32] =
            hex::decode("0fe478cb30a3bbfd13075549783c48472db97938c0b45010f8a518a0ca2f30a4")
                .unwrap()
                .try_into()
                .unwrap();
        let three: [u8; 32] =
            hex::decode("eff68fb36f1313ddff888fcd64452954f31c4651b907620d522d3f0725fe31dd")
                .unwrap()
                .try_into()
                .unwrap();

        let pair = create_npool();

        let res = pair.expected_exchange_extended(0, &one, &three).unwrap();
        assert_eq!(res.amount, 0);

        let res = pair
            .expected_exchange_extended(10000000, &one, &three)
            .unwrap();
        assert_eq!(res.amount, 9969721);

        let res = pair
            .expected_exchange_extended(12343, &one, &three)
            .unwrap();
        assert_eq!(res.amount, 12304);

        let res = pair.expected_exchange_extended(0, &one, &two).unwrap();
        assert_eq!(res.amount, 0);

        let res = pair.expected_exchange_extended(100, &one, &two).unwrap();
        assert_eq!(res.amount, 9900);

        let res = pair.expected_exchange_extended(100000, &one, &two).unwrap();
        assert_eq!(res.amount, 9969703);

        let res = pair
            .expected_exchange_extended(1000000, &one, &two)
            .unwrap();
        assert_eq!(res.amount, 99697028);
    }

    fn create_npool() -> StablePair {
        let td = vec![
            TokenDataInput {
                balance: 103103904425,
                decimals: 6,
            },
            TokenDataInput {
                balance: 10036272125538,
                decimals: 8,
            },
            TokenDataInput {
                balance: 100540680082,
                decimals: 6,
            },
        ];
        let ti = HashMap::from([
            (
                hex::decode("0b23ee4983cd1fa2372cf10abf04100414baa9dfd78cf79dcd06c90b60439fb8")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                0,
            ),
            (
                hex::decode("0fe478cb30a3bbfd13075549783c48472db97938c0b45010f8a518a0ca2f30a4")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                1,
            ),
            (
                hex::decode("eff68fb36f1313ddff888fcd64452954f31c4651b907620d522d3f0725fe31dd")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                2,
            ),
        ]);
        let a = AmplificationCoefficient {
            value: Natural::from(900u64),
            precision: Natural::ONE,
        };
        let fee = FeeParams {
            denominator: Natural::from(1000000u32),
            pool_numerator: Natural::from(3000u64),
            beneficiary_numerator: Natural::ZERO,
        };
        let pair = StablePair::new(td, ti, a, fee, 5711020512957239363328239).unwrap();

        pair
    }

    #[test]
    fn test_expected_withdraw_one_coin() {
        let pair = npool_for_single();

        let one = hex::decode("8276b9b9701c49addbb9dbb4e494cdc790df456c40600e4f72b1d39b50aba1c9")
            .unwrap()
            .try_into()
            .unwrap();

        let res = pair
            .expected_withdraw_liquidity_one_coin(10000, one)
            .unwrap();

        assert_eq!(res.lp_amount, 10000);
        assert_eq!(res.amounts[0], 9969999999996);
        assert_eq!(res.amounts[1], 0);
        assert_eq!(res.amounts[2], 0);

        assert_eq!(res.result_balances[0], 9999999990030000000004);
        assert_eq!(res.result_balances[1], 10000000000000000000000);
        assert_eq!(res.result_balances[2], 10000000000000000000000);

        assert_eq!(res.differences[0], 6636666666662);
        assert_eq!(res.differences[1], 3333333333334);
        assert_eq!(res.differences[2], 3333333333334);

        assert_eq!(res.pool_fees[0], 30000000000);
        assert_eq!(res.pool_fees[1], 0);
        assert_eq!(res.pool_fees[2], 0);

        assert_eq!(res.beneficiary_fees[0], 0);
        assert_eq!(res.beneficiary_fees[1], 0);
        assert_eq!(res.beneficiary_fees[2], 0);

        assert_eq!(res.sell[0], false);
        assert_eq!(res.sell[1], true);
        assert_eq!(res.sell[2], true);

        assert_eq!(res.invariant, 29999999990000000000000);

        assert_eq!(res.old_balances[0], 10000000000000000000000);
        assert_eq!(res.old_balances[1], 10000000000000000000000);
        assert_eq!(res.old_balances[2], 10000000000000000000000);

        let two = hex::decode("ad0ed85937d1ef51d04f28fb3621f911b58a9b78892dec0f830a946f4cc1585a")
            .unwrap()
            .try_into()
            .unwrap();

        let res = pair
            .expected_withdraw_liquidity_one_coin(14888888, two)
            .unwrap();

        assert_eq!(res.lp_amount, 14888888);
        assert_eq!(res.amounts[0], 0);
        assert_eq!(res.amounts[1], 14844220991847907);
        assert_eq!(res.amounts[2], 0);

        assert_eq!(res.result_balances[0], 10000000000000000000000);
        assert_eq!(res.result_balances[1], 9999985155779008152093);
        assert_eq!(res.result_balances[2], 10000000000000000000000);

        assert_eq!(res.differences[0], 4962962666666667);
        assert_eq!(res.differences[1], 9881258325181240);
        assert_eq!(res.differences[2], 4962962666666667);

        assert_eq!(res.pool_fees[0], 0);
        assert_eq!(res.pool_fees[1], 44666999950866);
        assert_eq!(res.pool_fees[2], 0);

        assert_eq!(res.beneficiary_fees[0], 0);
        assert_eq!(res.beneficiary_fees[1], 0);
        assert_eq!(res.beneficiary_fees[2], 0);

        assert_eq!(res.sell[0], true);
        assert_eq!(res.sell[1], false);
        assert_eq!(res.sell[2], true);

        assert_eq!(res.invariant, 29999985111112000000000);

        assert_eq!(res.old_balances[0], 10000000000000000000000);
        assert_eq!(res.old_balances[1], 10000000000000000000000);
        assert_eq!(res.old_balances[2], 10000000000000000000000);

        let three = hex::decode("d661a57349e301d15f8db3d0996c2a3c68c1cf147623b3a32fcb315e935db109")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair
            .expected_withdraw_liquidity_one_coin(9999999999999, three)
            .unwrap();

        assert_eq!(res.lp_amount, 9999999999999);
        assert_eq!(res.amounts[0], 0);
        assert_eq!(res.amounts[1], 0);
        assert_eq!(res.amounts[2], 9839687990770942485190);

        assert_eq!(res.result_balances[0], 10000000000000000000000);
        assert_eq!(res.result_balances[1], 10000000000000000000000);
        assert_eq!(res.result_balances[2], 160312009229057514810);

        assert_eq!(res.differences[0], 3333333333333000000000);
        assert_eq!(res.differences[1], 3333333333333000000000);
        assert_eq!(res.differences[2], 6506354657437942485190);

        assert_eq!(res.pool_fees[0], 0);
        assert_eq!(res.pool_fees[1], 0);
        assert_eq!(res.pool_fees[2], 15821275719351258134);

        assert_eq!(res.beneficiary_fees[0], 0);
        assert_eq!(res.beneficiary_fees[1], 0);
        assert_eq!(res.beneficiary_fees[2], 0);

        assert_eq!(res.sell[0], true);
        assert_eq!(res.sell[1], true);
        assert_eq!(res.sell[2], false);

        assert_eq!(res.invariant, 20000000000001000000000);

        assert_eq!(res.old_balances[0], 10000000000000000000000);
        assert_eq!(res.old_balances[1], 10000000000000000000000);
        assert_eq!(res.old_balances[2], 10000000000000000000000);
    }

    #[test]
    fn expected_deposit_one_coin() {
        let pair = npool_for_single();

        let one = hex::decode("8276b9b9701c49addbb9dbb4e494cdc790df456c40600e4f72b1d39b50aba1c9")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair.expected_one_coin_deposit_liquidity(one, 12345);

        assert!(res.is_none());

        let two = hex::decode("ad0ed85937d1ef51d04f28fb3621f911b58a9b78892dec0f830a946f4cc1585a")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair.expected_one_coin_deposit_liquidity(two, 12345);
        assert!(res.is_none());

        let three = hex::decode("d661a57349e301d15f8db3d0996c2a3c68c1cf147623b3a32fcb315e935db109")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair.expected_one_coin_deposit_liquidity(three, 12345);
        assert!(res.is_none());

        let res = pair
            .expected_one_coin_deposit_liquidity(three, 12131322211231)
            .unwrap();

        assert_eq!(res.lp_reward, 12094);
        assert_eq!(res.amounts[0], 0);
        assert_eq!(res.amounts[1], 0);
        assert_eq!(res.amounts[2], 12131322211231);

        assert_eq!(res.result_balances[0], 10000000000000000000000);
        assert_eq!(res.result_balances[1], 10000000000000000000000);
        assert_eq!(res.result_balances[2], 10000000012131322211231);

        assert_eq!(res.differences[0], 4043774070408);
        assert_eq!(res.differences[1], 4043774070408);
        assert_eq!(res.differences[2], 8087548140823);

        assert_eq!(res.pool_fees[0], 0);
        assert_eq!(res.pool_fees[1], 0);
        assert_eq!(res.pool_fees[2], 36393966633);

        assert_eq!(res.beneficiary_fees[0], 0);
        assert_eq!(res.beneficiary_fees[1], 0);
        assert_eq!(res.beneficiary_fees[2], 0);

        assert_eq!(res.sell[0], false);
        assert_eq!(res.sell[1], false);
        assert_eq!(res.sell[2], true);

        assert_eq!(res.invariant, 30000000012131322211225);
    }

    #[test]
    fn expected_deposit_one_coin_spend_amount() {
        let pair = npool_for_single();

        let one = hex::decode("8276b9b9701c49addbb9dbb4e494cdc790df456c40600e4f72b1d39b50aba1c9")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair.expected_deposit_spend_amount(one, 1488).unwrap();

        assert_eq!(res, 1492477432297);

        let two = hex::decode("ad0ed85937d1ef51d04f28fb3621f911b58a9b78892dec0f830a946f4cc1585a")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair
            .expected_deposit_spend_amount(two, 1488111111111)
            .unwrap();
        assert_eq!(res, 1492665202802920587939);

        let three = hex::decode("d661a57349e301d15f8db3d0996c2a3c68c1cf147623b3a32fcb315e935db109")
            .unwrap()
            .try_into()
            .unwrap();
        let res = pair.expected_deposit_spend_amount(three, 1).unwrap();
        assert_eq!(res, 1003009028);
    }

    fn npool_for_withdraws() -> StablePair {
        let td = vec![
            TokenDataInput {
                balance: 3000000000000000000000,
                decimals: 18,
            },
            TokenDataInput {
                balance: 1999999999999999999999,
                decimals: 18,
            },
            TokenDataInput {
                balance: 1000000000000000000000,
                decimals: 18,
            },
        ];
        let ti = HashMap::from([
            (
                hex::decode("836426076347dc312b950be54846d6415795b32bc664a08ceb034fd7358a2388")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                0,
            ),
            (
                hex::decode("9f556d1b20ed9125b2f3818bad6bb38dff45e4816e4ae151afea4f4f16ea2865")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                1,
            ),
            (
                hex::decode("e35765673ccc6c11889483944fcbc005af10c5980eada57f7f080442252f2e2a")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                2,
            ),
        ]);
        let a = AmplificationCoefficient {
            value: Natural::from(900u64),
            precision: Natural::ONE,
        };
        let fee = FeeParams {
            denominator: Natural::from(1000000u32),
            pool_numerator: Natural::from(3000u64),
            beneficiary_numerator: Natural::ZERO,
        };
        let pair = StablePair::new(td, ti, a, fee, 5990402341793730359785).unwrap();

        pair
    }

    fn npool_for_single() -> StablePair {
        let td = vec![
            TokenDataInput {
                balance: 10000000000000000000000,
                decimals: 18,
            },
            TokenDataInput {
                balance: 10000000000000000000000,
                decimals: 18,
            },
            TokenDataInput {
                balance: 10000000000000000000000,
                decimals: 18,
            },
        ];
        let ti = HashMap::from([
            (
                hex::decode("8276b9b9701c49addbb9dbb4e494cdc790df456c40600e4f72b1d39b50aba1c9")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                0,
            ),
            (
                hex::decode("ad0ed85937d1ef51d04f28fb3621f911b58a9b78892dec0f830a946f4cc1585a")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                1,
            ),
            (
                hex::decode("d661a57349e301d15f8db3d0996c2a3c68c1cf147623b3a32fcb315e935db109")
                    .unwrap()
                    .try_into()
                    .unwrap(),
                2,
            ),
        ]);
        let a = AmplificationCoefficient {
            value: Natural::from(900u64),
            precision: Natural::ONE,
        };
        let fee = FeeParams {
            denominator: Natural::from(1000000u32),
            pool_numerator: Natural::from(3000u64),
            beneficiary_numerator: Natural::ZERO,
        };
        let pair = StablePair::new(td, ti, a, fee, 30000000000000).unwrap();

        pair
    }

    #[test]
    fn test_int() {
        let int = eint::E256::from(12345u64);
        let mut arr = [0; 16];
        int.put(&mut arr);
        dbg!(int);
        let int2 = u128::from_le_bytes(arr);
        dbg!(int2);
        assert_eq!(int.u64(), int2 as u64);
    }

    #[test]
    fn test_deposit() {
        let token_data = vec![
            TokenDataInput {
                decimals: 6,
                balance: 914368094227,
            },
            TokenDataInput {
                decimals: 18,
                balance: 877809356052014984058390,
            },
            TokenDataInput {
                decimals: 6,
                balance: 897049561946,
            },
        ];

        let token_index = HashMap::from([([0; 32], 0), ([1; 32], 1), ([2; 32], 2)]);
        let pair = StablePair::new(
            token_data,
            token_index,
            AmplificationCoefficient {
                value: Natural::from(900u32),
                precision: Natural::ONE,
            },
            FeeParams {
                denominator: Natural::from(1000000u32),
                pool_numerator: Natural::from(250u64),
                beneficiary_numerator: Natural::from(250u64),
            },
            2689226563382761,
        )
        .unwrap();

        let res = pair
            .expected_deposit_spend_amount([1; 32], 1_000_000_000)
            .unwrap();
        println!("{}", res);
        assert_eq!(1000476879669130732, res);
    }
}
