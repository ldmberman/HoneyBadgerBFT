//! The module contains utilities for:
//!
//! - signing the given byte array with a given secret key
//! - calculating public key from the given secret key
//! - verifying the given signature with the given public key
//!
//! TODO - encryption and decryption;
//! TODO - a synchronous algorithm for dealer-less distributed key generation;
//! TODO - threshold encryption and decryption.
//!
//! The cryptography is based on the BLS12-381 curve.
//!
//! The implementation roughly repeats the corresponding section of the HoneyBadgerBFT implementation
//! by POA Network. For instance, see https://github.com/poanetwork/hbbft/blob/master/src/sync_key_gen.rs.
#![crate_type = "lib"]

extern crate libc;

extern crate threshold_crypto;

use std::slice;
use std::mem;
use std::vec::{Vec};
use libc::{c_uchar};
use threshold_crypto::{
    Fr,
    FrRepr,
    pairing::{PrimeField},
    Signature,
    SecretKey,
    PublicKey,
    SIG_SIZE,
    PK_SIZE,
};

#[no_mangle]
pub unsafe extern fn sign(message: *const c_uchar, message_size: usize,
                          signature_bytes: &mut *mut c_uchar,
                          sk_fr_1: u64, sk_fr_2: u64, sk_fr_3: u64, sk_fr_4: u64) -> usize {
    let secret_key = secret_key_from_field_element(
        &[sk_fr_1, sk_fr_2, sk_fr_3, sk_fr_4,]
    );

    let message_slice = slice::from_raw_parts(message, message_size);
    let signature: Signature = secret_key.sign(message_slice);

    let mut slice = Vec::with_capacity(SIG_SIZE);
    for element in signature.to_bytes().iter() {
        slice.push(*element);
    }
    slice.shrink_to_fit();
    *signature_bytes = slice.as_mut_ptr();
    let size = slice.len();
    mem::forget(slice);

    size
}

#[no_mangle]
pub unsafe extern fn arr_free(arr: *mut c_uchar, size: usize) {
    if arr.is_null() {
        return;
    }

    Vec::from_raw_parts(arr, size, size);
}

#[no_mangle]
pub unsafe extern fn public_key_from_secret_key(sk_fr_1: u64, sk_fr_2: u64, sk_fr_3: u64, sk_fr_4: u64,
                                                public_key_bytes: &mut *mut c_uchar) -> usize {
    let secret_key: SecretKey = secret_key_from_field_element(
        &[sk_fr_1, sk_fr_2, sk_fr_3, sk_fr_4],
    );
    let public_key = secret_key.public_key();

    let mut slice = Vec::with_capacity(PK_SIZE);
    for element in public_key.to_bytes().iter() {
        slice.push(*element);
    }

    slice.shrink_to_fit();
    *public_key_bytes = slice.as_mut_ptr();
    let size = slice.len();
    mem::forget(slice);

    size
}

#[no_mangle]
pub unsafe extern fn verify(message: *const c_uchar, message_size: usize,
                            signature: *const c_uchar, signature_size: usize,
                            public_key: *const c_uchar, pk_size: usize) -> bool {
    let message_slice = slice::from_raw_parts(message, message_size);
    let signature_slice = slice::from_raw_parts(signature, signature_size);
    let pk_slice = slice::from_raw_parts(public_key, pk_size);

    assert!(signature_size == SIG_SIZE);
    let mut signature_array: [u8; SIG_SIZE] = [0; SIG_SIZE];
    for (place, element) in signature_array.iter_mut().zip(signature_slice.iter()) {
        *place = *element;
    }
    let signature = Signature::from_bytes(signature_array).unwrap();

    assert!(pk_size == PK_SIZE);
    let mut pk_array: [u8; PK_SIZE] = [0; PK_SIZE];
    for (place, element) in pk_array.iter_mut().zip(pk_slice.iter()) {
        *place = *element;
    }
    let public_key = PublicKey::from_bytes(pk_array).unwrap();
    public_key.verify(&signature, message_slice)
}

fn secret_key_from_field_element(field_element: &[u64; 4]) -> SecretKey {
    let mut fr = Fr::from_repr(FrRepr(*field_element)).unwrap();
    SecretKey::from_mut(&mut fr)
}

#[cfg(test)]
mod threshold_crypto_ffi_tests {
    use libc::{c_uchar};

    use {
        sign,
        verify,
        public_key_from_secret_key,
        arr_free,
    };

    #[test]
    fn encrypt_decrypt_message() {
        let message_str = "Bla bla bla";
        let message_size = message_str.len();
        let message = message_str.as_ptr();

        unsafe {
            let mut signature_slice = Vec::new();
            let signature: &mut *mut c_uchar = &mut signature_slice.as_mut_ptr();
            let signature_size = sign(
                message,
                message_size,
                signature,
                1, 2, 3, 4,
            );

            let mut pk_slice = Vec::new();
            let pk: &mut *mut c_uchar = &mut pk_slice.as_mut_ptr();
            let pk_size = public_key_from_secret_key(
                1, 2, 3, 4,
                pk,
            );

            assert!(
                verify(
                    message,
                    message_size,
                    *signature,
                    signature_size,
                    *pk,
                    pk_size,
                ),
            );

            arr_free(*pk, pk_size);
            arr_free(*signature, signature_size);
        }
    }
}