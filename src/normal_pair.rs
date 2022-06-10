use serde::{Deserialize, Serialize};

use crate::utils::mul_divc;

#[derive(Serialize, Deserialize)]
pub struct Pair {
    left_amount: u128,
    right_amount: u128,

    fee_numerator: u128,
    fee_denominator: u128,
}

impl Pair {
    pub fn new(left_amount: u128, right_amount: u128) -> Self {
        Self {
            left_amount,
            right_amount,
            fee_numerator: 3,
            fee_denominator: 1000, // 0.03%
        }
    }

    pub fn is_empty(&self) -> bool {
        self.left_amount == 0 && self.right_amount == 0
    }

    pub fn expected_exchange(&self, direction: Direction, a_amount: u128) -> Option<SwapResult> {
        let (a_pool, b_pool) = match direction {
            Direction::LeftToRight => (self.left_amount, self.right_amount),
            Direction::RightToLeft => (self.right_amount, self.left_amount),
        };

        let a_fee = mul_divc(a_amount, self.fee_numerator, self.fee_denominator)?;
        let new_a_pool = a_pool + a_amount;
        let new_b_pool = mul_divc(a_pool, b_pool, new_a_pool - a_fee)?;
        let expected_b_amount = b_pool - new_b_pool;

        Some(SwapResult {
            amount: expected_b_amount,
            fee: a_fee,
        })
    }

    pub fn expected_spend_amount(
        &self,
        b_amount: u128,
        a_pool: u128,
        b_pool: u128,
    ) -> Option<SwapResult> {
        let fee_d_minus_n = self.fee_denominator - self.fee_numerator;

        let new_b_pool = b_pool - b_amount;
        let new_a_pool = mul_divc(a_pool, b_pool, new_b_pool)?;
        let expected_a_amount = mul_divc(new_a_pool - a_pool, self.fee_denominator, fee_d_minus_n)?;
        let a_fee = mul_divc(expected_a_amount, self.fee_numerator, self.fee_denominator)?;

        Some(SwapResult {
            amount: expected_a_amount,
            fee: a_fee,
        })
    }
}

pub enum Direction {
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Clone, Copy)]
pub struct SwapResult {
    pub amount: u128,
    pub fee: u128,
}

#[cfg(test)]
mod test {
    #[test]
    fn test_swap() {
        let left = 266342825246179940;
        let right = 58776831;
        let pair = super::Pair::new(left, right);
        let got = pair
            .expected_exchange(super::Direction::LeftToRight, 123123123123123)
            .unwrap();
        assert_eq!(got.amount, 27076);
        assert_eq!(got.fee, 369369369370);
    }

    #[test]
    fn test_muldivc() {
        let r = super::mul_divc(3, 7, 2).unwrap();
        assert_eq!(r, 11);
    }
}
