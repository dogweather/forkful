---
title:                "Rust: पैटर्न से मेल खाने वाले अक्षरों को हटाना"
simple_title:         "पैटर्न से मेल खाने वाले अक्षरों को हटाना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyun

Kabhi kabhi humare code mein unwanted characters ho jate hain aur hum unko delete karna chahte hain taaki humara code efficient aur clean rahe. Is situation mein hum characters ko delete karne ka ek pattern use kar sakte hain. Is blog post mein hum dekhenge ki hum characters ko delete kaise kar sakte hain Rust programming language mein.

## Kaise Kare

Hum characters ko delete karne ke liye `str::replace` function ka use kar sakte hain. Is function mein hum `pattern` aur `replace_with` arguments pass karte hain. Jaise ki is coding example mein dikhaya gaya hai:

```Rust
let string = String::from("Hindi me programming sikhna accha lagta hai.");
let new_string = string.replace("a",""); 
```

Is coding example mein humne `a` character ko replace nahi kiya lekin usko delete kar diya. Is code ki output `Hindi me progrming sihkn acch lgt h.i` hogi.

## Gehri Jankari

`str::replace` function characters ko delete karne ke liye kafi useful hai. Hum is function ka use karke specific patterns ke according characters ko delete bhi kar sakte hain. Iske alawa, hum regex pattern ka bhi use kar sakte hain character deletion ke liye. Rust mein regex pattern ke liye hum `regex` crate ka use kar sakte hain.

## Dekhe Bhi

- [Rust Language Documentation](https://www.rust-lang.org/hi)
- [regex Crate Documentation](https://crates.io/crates/regex)
- [Rust By Example](https://doc.rust-lang.org/rust-by-example/)

Asha karte hain ki ab aapko characters ko delete karne ke liye pattern use karne ka tarika samajh aa gaya hai. Agar aapko koi aur programming topic par article padhna hai to humare "See Also" section mein diye gaye links ko follow karein. Happy coding!