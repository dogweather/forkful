---
title:                "टेक्स्ट ढूंढ़ना और प्रतिस्थापन करना"
html_title:           "Rust: टेक्स्ट ढूंढ़ना और प्रतिस्थापन करना"
simple_title:         "टेक्स्ट ढूंढ़ना और प्रतिस्थापन करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapne kabhi text ko replace karna chaha hai? Sayad aapko lagta ho gya hai ki yeh kam bohot boring hai. Lekin Rust mein text ko search aur replace karna behad asaan hai. Is article mein hum Rust ke current version ki madad se text ko search aur replace karne ke tarike ke bare mein baat karenge.

## Kaise Kare

Sabse pehle, hume `std::string::String` ko import karna hoga. Isse hume text ko manipulate karne mein madad milegi. Fir hum `replace()` function ko use karke text ko replace kar sakte hai. Yeh function do arguments lete hai - pehla hai jo hum replace karna chahte hai aur doosra hai jo humare dwara replace kiya gaya text hai.

```
Rust
let mut text = String::from("Hello world!");
text.replace("world", "Rust");
```

Output: "Hello Rust!"

Toh yeh hai basic tarike se text replace karne ka. Lekin aap isse aur bhi advanced bana sakte hai. Jaise ki, multiple words ko replace kar sakte hai, regex ka use karke bhi replace kar sakte hai aur bhi bahut kuch.

## Gehri Jhaank

Jaise ki humne dekha, `replace()` function ek string ko dusre string se replace karta hai. Lekin iske alawa bhi Rust mein kuch aur options available hai. Jaise ki `replace_range()` function jo ki ek specific range mein text ko replace karta hai.

```
Rust
let mut text = String::from("Rust is awesome!");
text.replace_range(0..4, "I");
```

Output: "I is awesome!"

Iske alawa, `clone_from()` function bhi hai jo ki ek string ko dusri string se replace karta hai. Lekin isme hume do arguments provide karna hoga - source string aur destination string.

```
Rust
let mut source = String::from("Hello");
let mut dest = String::from("Rust");
dest.clone_from(&source);
```

Output: "Hello"

## Dekhein Bhi

Agar aap aur jyada advanced tarike sikhna chahte hai toh maine kuch links niche provide kiye hai.

- [Rust String Documentation](https://doc.rust-lang.org/std/string/index.html)
- [Rust By Example](https://doc.rust-lang.org/stable/rust-by-example/index.html)
- [Rust Reddit Community](https://www.reddit.com/r/rust/)

Toh ab aap text ko search aur replace karne ke tarike sikh chuke hai. Rust mein text manipulation kaafi powerful hai aur aage aur bhi possibilities hai. Happy coding!