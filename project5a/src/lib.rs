pub mod mandelbrot;
pub mod points;

/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1;
    } else if n == 0 {
        return 0;
    }
    return n + gauss(n - 1)
}

/**
 * Adds one to the referenced integer.
 */
pub fn add_one(n: &mut i32) {
    *n = *n + 1;
}

/**
 * Modifies the input string so that every character is replaced with 'a'.
 * See https://doc.rust-lang.org/std/string/struct.String.html.
 */
pub fn rewrite_string(s: &mut String) {
    let length = s.len();
    *s = String::from("");
    for x in 0..length {
        s.push_str("a")
    }
    println!("{}", s);
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array.
    Index 0 stores the high-order bit.

    Conversion is unsigned (all place values are positive).
    
    Ex: to_decimal of [true, false, true, false] returns 10
**/
pub fn to_decimal(ls: [bool; 32]) -> u32 {
    let length = ls.len();
    let mut sum = 0;
    for x in 0..length {
        if ls[x] {
            let new_value = 2_u32.pow((length - x - 1) as u32);
            sum += new_value;
        }
    }
    return sum;
}

/**
 * The Collatz conjecture (https://en.wikipedia.org/wiki/Collatz_conjecture) is that
 * when one repeatedly applies a function 'f' to a positive integer,
 * the output will eventually be 1. 'f' is defined as follows:
 * 
 * f(n) = n/2 if n is even
 * f(n) = 3n + 1 if n is odd
 * 
 * The function collatz(n) returns the number of times one must apply f to n to get 1.
 * For example, f(4) = 2, f(2) = 1. That is, it took 2 applications of f to get from 4 to 1.
 * Therefore, collatz(4) = 2.
*/
pub fn collatz(mut n: u64) -> u64 {
    if n == 1 {
        return 0;
    }
    if n % 2 == 0 {
        return 1 + collatz(n/2)
    } else {
        return 1 + collatz( 3 * n + 1)
    }
}

/**
 * Returns a vector containing collatz(1), collatz(2), ..., collatz(n).
 */
pub fn collatz_times(n: u64) -> Vec<u64> {
    let mut v = Vec::new();
    for x in 0..n {
        v.push(collatz(x + 1));
    }
    return v;
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let length = lst.len();
    let mut v = Vec::new();
    for x in 0..length {
        v.push(lst[(x + 1) % (length)])
    }
    return v;
}

/**
 * Returns a new string whose contents is the concatenation of the two input slices.
 */
pub fn concatenate_strings(s1: &str, s2: &str) -> String {
    let mut new_string = String::from("");
    new_string.push_str(s1);
    new_string.push_str(s2);
    return new_string;
}
