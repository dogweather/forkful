---
title:                "स्ट्रिंग को लोअर केस में रूपांतरण करना"
html_title:           "Rust: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kya Aur Kyun?
String ko lower case mein convert karna ek common programming task hai. Isme, hum ek string ko lowercase letters mein badal dete hai taaki use alag se handle kiya ja sake. Jaise ki, agar koi user input uppercase letters mein deta hai toh hum input ko lower case mein convert kar sakte hai aur phir usko compare ya manipulate kar sakte hai. Is prakar, hume apne code ko flexible aur robust banane ke liye string ko lower case mein convert karna zaruri hota hai.

## Kaise Karein?
```Rust
let input: String = String::from("HELLO WORLD");
let lowercase: String = input.to_lowercase();

println!("Input: {}", input); // Output: HELLO WORLD
println!("Lowercase: {}", lowercase); // Output: hello world
```

Is coding example mein hum dekh sakte hai ki hum ek string variable ko `to_lowercase()` function ka use karke lower case mein convert kar sakte hai. Yaha `String::from()` hume ek string value create karne ka tareeka deta hai aur `println!` function hume terminal mein output print karne ka tareeka deta hai.

## Gehri Jhaanki
Jab hum strings ko lowercase mein convert karte hai, hum ek ASCII table ka use karte hai. ASCII table ek character encoding scheme hai jisme har character ko ek numerical value assign kiya gaya hai. Ye values strings ko compare aur manipulate karne mein bahut helpful hote hai. Ek aur tareeka string ko lower case mein convert karne ka hai `chars()` function ka use karna aur phir unn characters ko lower case mein convert karke ek nayi string bana lena.

## Aur Dekhen
Agar aapko string manipulation aur character encoding scheme jaise concepts mein aur gahrai se jaan na hai, toh aap niche diye gaye links ko check kar sakte hai:
- [Amanuscript](https://amanuscript.com/blog/programming-basics-strings/)
- [Rust Documentation on `to_lowercase()`](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)

Dhanyavaad! Happy coding!