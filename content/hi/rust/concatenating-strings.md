---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Rust: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyun

Agar aap ek programmer hai, toh aapne strings ko apne code mein kai baar use kiya hoga. Kabhi kabhi humein do alag alag strings ko ek saath jodna hota hai, jaise ki "Hello" aur "World" ko jodkar "Hello World" banana. Isko hum string concatenation kehte hain. Is technique ka use karke hum apne code mein strings ko dynamic tarike se use kar sakte hain aur apne program ko aur bhi powerful bana sakte hain. Is article mein hum dekhenge ki Rust mein string concatenation kaise kiya jaata hai aur iske kya benefits hai.

## Kaise

```
fn main() {
    let str1 = "Hello";
    let str2 = "World";
    let combined_string = str1.to_string() + " " + str2;
    println!("{}", combined_string);
}
```

Is code mein humne `str1` aur `str2` variables mein "Hello" aur "World" strings ko store kiya hai. Fir humne `combined_string` variable mein do strings ko `+` operator ka use karke concatenate kiya hai. Agar hum is code ko run karenge toh humein output mein "Hello World" milega. Rust mein `.to_string()` method se hum string ko `String` type mein convert kar sakte hain jisse hum ise concatenate kar sakein.

## Deep Dive

Rust mein humein `String` type mein aur `&str` type mein strings ko use karna hota hai. `String` type mutable hai aur usmein hum koi bhi changes kar sakte hain, jabki `&str` type immutable hai aur usmein hum changes nahi kar sakte. Isliye hum concatenate karte waqt `to_string()` method ka use karte hain taaki hum strings ko modify kar sakein.

Iske alawa, Rust mein hum `format!` macro ka bhi use kar sakte hain. Is macro mein hum multiple strings ko ek saath combine kar sakte hain. Below code mein humne `"Hello"` aur `"World"` ko `format!` macro se combine kiya hai aur output mein humein "Hello World" milega.

```
fn main() {
    let combined_string = format!("{} {}", "Hello", "World");
    println!("{}", combined_string);
}
```

## Dekho Bhi

- [Rust Strings Documentation](https://doc.rust-lang.org/std/string/index.html)
- [Learn Rust in Y Minutes - Strings](https://learnxinyminutes.com/docs/rust/#strings)
- [Rust Book - Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)

"Chalti Bhais" Image Credit: https://pixabay.com/illustrations/rust-digital-art-stracture-science-426761/