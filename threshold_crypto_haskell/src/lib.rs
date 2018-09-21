#![crate_type = "lib"]

extern crate libc;
extern crate rand;
extern crate threshold_crypto;

use libc::c_char;

use threshold_crypto::SecretKeySet;


#[no_mangle]
pub extern fn generate_secret_shares(number_of_parties: usize) -> *const c_char {
    let mut rng = rand::thread_rng();
    let num_faulty = (number_of_parties - 1) / 3;

    let sk_set = SecretKeySet::random(num_faulty, &mut rng);

    sk_set.secret_key_share(0).reveal().as_ptr() as (*const c_char)
}
