---
title:                "कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
html_title:           "Rust: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट पढ़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapne kabhi command line arguments ka istemal kiya hai? Agar haan, to aap jaante honge ki ye programming mein bahut hi zaroori aur upyogi hote hain. Command line arguments se hum hamare program ko customize kar sakte hain aur use user-friendly bana sakte hain. Isliye aaj hum Rust mein command line arguments ko kaise padh sakte hain, uske bare mein baat karenge.

## Kaise Kare

Sabse pehle hume ek "args" naam ka "Vec" (vector) create karna hoga. Iske liye hum "std::env::args()" ka use karenge. Is function se hum command line arguments ko ek vector ke roop mein access kar sakte hain.

```Rust
let args: Vec<String> = std::env::args().collect();
```

Yaha, "Vec<String>" string vector ko represent karta hai aur "collect()" function hume string vector banane mein help karta hai.

Ab hum apne program mein command line arguments ko print kar sakte hain. Iske liye hum for loop ka use karenge aur "args" vector ke har element ko print karenge.

```Rust
for argument in args {
    println!("{}", argument);
}
```

Output:

```
./my_program
argument1
argument2
```

Agar hume shuru ke teen arguments ko ignore karna hai aur sirf user dwara diye gaye arguments ko print karna hai, to hum "skip()" function ka bhi use kar sakte hain.

```Rust
for argument in args.skip(3) {
    println!("{}", argument);
}
```

Output:

```
argument1
argument2
```

## Gehri Jhaank

Humne dekha ki command line arguments ko kaise padha jata hai aur unka istemal kaise kiya jaata hai. Lekin kya aap jaante hain ki hum command line arguments ko kisi bhi data type mein convert bhi kar sakte hain? Haan, Rust mein hum "to_string()" aur "parse()" functions ka istemal karke string ko integer ya float mein convert kar sakte hain.

```Rust
let num1 = args[1].to_string().parse::<i32>().unwrap();
let num2 = args[2].to_string().parse::<i32>().unwrap();
```

Yaha, humne input ki string ko "parse()" function se integer mein convert kiya. Agar input ko integer se float mein convert karna hai, to hum "parse::<f32>()" ka istemal kar sakte hain. Lekin dhyan rakhein, string ko sahi data type mein convert karne ke liye, input ki string mein sahi format hona bahut zaroori hai.

## Dekhiye Bhi

- [Rust command line arguments official documentation](https://doc.rust-lang.org/std/env/fn.args.html)
- [Rust string to integer conversion official documentation](https://doc.rust-lang.org/std/primitive.str.html#method.parse)
- [Detailed tutorial on reading command line arguments in Rust](https://www.educative.io/edpresso/how-to-read-command-line-arguments-in-rust)