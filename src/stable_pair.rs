use std::collections::HashMap;

use anyhow::anyhow;
use malachite_base::num::{arithmetic::traits::Pow, basic::traits::*};
use malachite_nz::natural::Natural;

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

    pub fn update_balances(&mut self, balances: Vec<u128>) -> Result<(), anyhow::Error> {
        for (i, balance) in balances.into_iter().enumerate() {
            let token = self
                .token_data
                .get_mut(i)
                .ok_or_else(|| anyhow!("invalid tokens len"))?;
            token.balance = balance.into();
        }
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

    // function _xp_mem(uint128[] _balances) internal view returns(uint256[]) {
    // uint256[] result = new uint256[](0);
    //
    // for (uint8 i = 0; i < N_COINS; i++) {
    // result.push(math.muldiv(tokenData[i].rate, _balances[i], PRECISION));
    // }
    //
    // return result;
    // }

    fn xp_mem(&self, balances: &[Natural]) -> Option<Vec<Natural>> {
        let mut result = Vec::new();
        for (i, balance) in balances.iter().enumerate() {
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

        let d0 = self.get_d(&self.xp_mem(&old_balances)?)?;

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

        let d1 = self.get_d(&self.xp_mem(&new_balances)?)?;
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

            let d2 = self.get_d(&self.xp_mem(&new_balances)?)?;
            lp_reward = (&mul_div(&self.lp_supply, &(d2 - &d0), &d0)?)
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
    fn test_expecte() {}

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
}
