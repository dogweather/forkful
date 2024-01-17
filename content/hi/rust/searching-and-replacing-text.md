---
title:                "टेक्स्ट को खोजें और बदलें"
html_title:           "Rust: टेक्स्ट को खोजें और बदलें"
simple_title:         "टेक्स्ट को खोजें और बदलें"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kya & Kyun?
"Searching and replacing text" ek aam task hai jo kisi bhi programming language mein important hai. Jaise ki naam se hi pata chalta hai, isme hum kisi text mein se kuch specific pattern ya string ko dhoondhte hain aur usko dusre pattern ya string se replace karte hain. Kuch kaaran hai jo hume text ko search aur replace karne ke liye majboor karte hain, jaise ki code optimization, bug fixing aur code readability.

## Kaise Karein:
Rust mein text ko search aur replace karna kaafi aasan hai. Iske liye hum "replace" method ka istemal karte hain. Neeche diye gaye code blocks mein aap dekh sakte hain ki kaise hum ek string ko dusre string se replace kar sakte hain:

```
// String ko replace karna
let original_string = "Hello World";
let new_string = original_string.replace("World", "Rust");
println!("{}", new_string);
// Output: "Hello Rust"

// Character ko replace karna
let original_string = "Rust is fun";
let new_string = original_string.replace("R", "P");
println!("{}", new_string);
// Output: "Pust is fun"
```
Neeche diye gaye code block mein hum ek specific pattern ko bhi replace kar sakte hain:

```
let original_string = "Rust is awesome";
let new_string = original_string.replace("awesome", "cool");
println!("{}", new_string);
// Output: "Rust is cool"
```

## Gehri Jhaank:
Searching and replacing text ka concept programming languages mein kaafi pehle se kaam kiya jaata hai. Iske liye kuch popular alternatives hain jaise ki regular expressions (regex) aur string search algorithms. Rust mein bhi regex ka istemal text ko search aur replace karne ke liye kiya ja sakta hai.

Rust mein "replace" method ke peeche kaam karne ka algorithm "Boyer-Moore" hai. Ye kaafi efficient hai aur kaafi jyada byte code generate karta hai as compared to other methods.

## Aur Dekhein:
Agar aapko Rust mein string processing se related aur jaankari chahiye, to aap in resources ko refer kar sakte hain:

1. Official Rust documentation for string manipulation: [https://doc.rust-lang.org/std/string/index.html](https://doc.rust-lang.org/std/string/index.html)

2. Rust By Example chapter on string manipulation: [https://doc.rust-lang.org/stable/rust-by-example/std/str.html](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)

3. Official Rust blog post on string performance improvements in Rust 1.36: [https://blog.rust-lang.org/2019/07/04/Rust-1.36.0.html](https://blog.rust-lang.org/2019/07/04/Rust-1.36.0.html)