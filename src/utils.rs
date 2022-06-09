use malachite_base::num::basic::traits::*;
use malachite_nz::natural::Natural;
use num_bigint::BigUint;
use std::borrow::Borrow;

pub fn mul_divc(val: u128, num: u128, denom: u128) -> Option<u128> {
    if denom == 0 {
        return None;
    }
    let val = eint::E256::from(val);
    let num = eint::E256::from(num);
    let denom = eint::E256::from(denom);

    let r = (val * num + (denom - eint::E256::from(1))) / denom; // biguint is used to check overflow, using plain u128
    if r.1 != eint::E128(0) {
        return None;
    }

    Some(r.0 .0)
}

pub fn mul_divc_native(val: u128, num: u128, denom: u128) -> Option<u128> {
    if denom == 0 {
        return None;
    }

    let r = (val.checked_mul(num)? + (denom - 1)).checked_div(denom)?; // biguint is used to check overflow, using plain u128

    Some(r)
}

pub fn mul_divc_dec(val: u128, num: u128, denom: u128) -> Option<BigUint> {
    if denom == 0 {
        return None;
    }
    let val = BigUint::from(val);
    let num = BigUint::from(num);
    let denom = BigUint::from(denom);

    let r = (&val * &num + (&denom - BigUint::from(1u128))) / denom; // biguint is used to check overflow, using plain u128
    if r >= BigUint::from(u128::max_value()) {
        return None;
    }
    Some(r)
}

pub fn mul_divc_mal<V, NUM, DENOM>(val: V, num: NUM, denom: DENOM) -> Option<Natural>
where
    V: Borrow<Natural>,
    NUM: Borrow<Natural>,
    DENOM: Borrow<Natural>,
{
    let val = val.borrow();
    let num = num.borrow();
    let denom = denom.borrow();

    if denom == &Natural::ZERO {
        return None;
    }
    let r = (val * num + (denom - &Natural::ONE)) / denom; // biguint is used to check overflow, using plain u128
    if r >= Natural::from(u128::MAX) {
        return None;
    }
    Some(r)
}

pub fn mul_div<V, NUM, DENOM>(val: V, num: NUM, denom: DENOM) -> Option<Natural>
where
    V: Borrow<Natural>,
    NUM: Borrow<Natural>,
    DENOM: Borrow<Natural>,
{
    let val = val.borrow();
    let num = num.borrow();
    let denom = denom.borrow();

    if denom == &Natural::ZERO {
        return None;
    }
    let r = val * num + ((denom >> 1) / denom);
    if r > Natural::from(u128::MAX) {
        return None;
    }
    Some(r)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_eq() {
        let mut a = 1;
        loop {
            let res = mul_divc(a, a, 3);
            let res_native = mul_divc_native(a, a, 3);
            if res != res_native {
                println!("{}", a);
                println!("{:?}", res);
                println!("{:?}", res_native);
                panic!("");
            }
            a *= 2;
            if res.is_none() {
                break;
            }
        }
    }

    #[test]
    fn test() {
        assert_eq!(mul_divc(3, 7, 2).unwrap(), 11);
        assert_eq!(mul_divc_native(3, 7, 2).unwrap(), 11);
    }
}
