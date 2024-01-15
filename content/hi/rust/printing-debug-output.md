---
title:                "डिबग आउटपुट प्रिंट करना"
html_title:           "Rust: डिबग आउटपुट प्रिंट करना"
simple_title:         "डिबग आउटपुट प्रिंट करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyun

Jab hum programming mein kaam karte hai, hume kai baar apne code mein bugs aur errors ko find karne ki zarurat padti hai. Iske liye hum print debugging ka istemal karte hai. Yeh ek aasan aur zaruri technique hai jiska istemal hum apne code ko improve karne aur sahi se chalane mein kar sakte hain.

## Kaise Karein

Agar aapko apne code mein kisi particular value ya object ki debugging karni hai, to aap ```println!()``` macro ka istemal kar sakte hain. Ismein aap apni desired value ko pass karke output ko console mein print kar sakte hain.

For example:

```Rust
fn main() {
    let x = 5;
    println!("The value of x is: {}", x);
}
```

Yeh code output ke roop mein ```The value of x is: 5``` dega.

Agar aap multiple values ko print karna chahte hain, to aap ```println!()``` macro mein multiple placeholders ka istemal kar sakte hain aur phir unke corresponding values ko comma se separate kar sakte hain.

For example:

```Rust
fn main() {
    let name = "John";
    let age = 25;
    println!("My name is {} and I am {} years old.", name, age);
}
```

Yeh code output ke roop mein ```My name is John and I am 25 years old.``` dega.

## Deep Dive

Print debugging ke liye ek aur useful macro hai - ```dbg!()```. Yeh macro hume not only value ko print karne ke liye, balki uska entire expression ko evaluate karne aur output mein display karne ke liye bhi allow karta hain.

For example:

```Rust
fn main() {
    let x = 10;
    let y = 5;
    let product = x * y;
    dbg!(product);
}
```

Yeh code output ke roop mein ```product = 50``` dega.

Iske alawa, hum apne code mein condition statements aur loops mein bhi print debugging ka istemal kar sakte hain. Isse hume code ke flow ko samajhne mein madad milti hai aur sahi output ko identify karne mein help milti hai.

See Also

- [The Rust Programming Language](https://www.rust-lang.org/learn)
- [Debugging in Rust](https://doc.rust-lang.org/book/ch12-00-an-io-project.html#debugging-with-print) 
- [Rust By Example: Print Debugging](https://doc.rust-lang.org/rust-by-example/hello/print.html)