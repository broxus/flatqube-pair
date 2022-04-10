use bigdecimal::num_bigint::{BigUint, ToBigUint};
use bigdecimal::{One, ToPrimitive};

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

    // uint128 a_fee = math.muldivc(a_amount, fee_numerator, fee_denominator);
    //
    // uint128 new_a_pool = a_pool + a_amount;
    // uint128 new_b_pool = math.muldivc(a_pool, b_pool, new_a_pool - a_fee);
    // uint128 expected_b_amount = b_pool - new_b_pool;
    //
    // return (expected_b_amount, a_fee);

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
}

fn mul_divc(val: u128, num: u128, denom: u128) -> Option<u128> {
    let val = val.to_biguint()?;
    let num = num.to_biguint()?;
    let denom = denom.to_biguint()?;
    let r = (val * num + (denom.clone() - BigUint::one())) / denom; // biguint is used to check overflow, using plain u128

    r.to_u128()
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