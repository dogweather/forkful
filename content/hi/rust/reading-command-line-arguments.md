---
title:    "Rust: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स पढ़ना।"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Kyun

Ham sabhi jante hain ki Rust ek powerful programming language hai jo performance, concurrency, aur memory safety ke liye famous hai. Command line arguments ko read karna bhi Rust ke programmers ke liye ek important skill hai, kyunki isse aap apne program ko dynamic bana sakte hain aur user input ke sath interact kar sakte hain. Isliye aaj hum command line arguments ko read karne ke bare mein baat karenge.

## Kaise Karein

Pehle hum `std::env::args()` function se arguments ko collect karenge. Fir hum `collect()` function ka use karke ise vector mein convert karenge. Iss vector mein hum chahe jitne bhi arguments ko store kar sakte hain. Yeh ek simple example hai:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    println!("Pehla argument: {}", args[0]);
    println!("Dusra argument: {}", args[1]);
}
```

Agar hum terminal mein `cargo run hello world` command run karte hain, toh output yeh hoga:

```
Pehla argument: target/debug/command-line-args
Dusra argument: hello
```
Fir hum `std::env::args_os()` function ka use karke arguments ko kisi aur type mein bhi read kar sakte hain, jaise ki `OsString` ya `CString`. Iske liye hume ek loop bhi use karna padega. Ek example is tarah se hai:

```Rust
use std::env;

fn main() {
    let args: Vec<_> = env::args_os().collect();

    for argument in args {
        let arg_str = argument.to_string_lossy();

        println!("{}", arg_str);
    }
}
```

Iss code mein hum `to_string_lossy()` function ka use kar rahe hain jisse hum `OsString` type ko `String` type mein convert kar sakte hain.

## Gehri Jankari

Command line arguments read karne ke liye, hume `std::env` module ka use karna padta hai. Iss module mein hume `args()` aur `args_os()` jaise functions milte hain. `std::env` module ke alawa bhi kuch third-party crates available hain, jaise ki `clap` aur `structopt`, jo command line argument parsing mein madad karte hain.

Iss tarah se, command line arguments ko read karna Rust mein kaafi aasaan hai aur isse aap apne program ke flexibility aur user-friendly nature ko enhance kar sakte hain.

## Dekhiye Bhi

- [Official Rust documentation on `std::env` module](https://doc.rust-lang.org/std/env/index.html)
- [`clap` crate for command line argument parsing](https://docs.rs/clap/2.33.3/clap/)
- [`structopt` crate for defining CLI arguments as structs](https://docs.rs/structopt/0.3.21/structopt/)