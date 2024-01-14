---
title:                "Rust: स्ट्रिंग को लोअर केस में कनवर्ट करना"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Kyun

Kya aapne kabhi String ko lower case mein convert karne ki koshish ki hai? Yeh ek common programming task hai, jo bade se bade programs mein bhi istemal hoti hai. Kya aap jaante hain ki Rust mein String ko lower case mein convert karna bohot hi aasan hai? Iss blog post mein hum aapko batayenge ki kyun aur kaise aap apne Rust projects mein string ko lower case mein convert kar sakte hain.

## Kaise Karein

Rust mein string ko lower case mein convert karne ke liye, hum `to_lowercase()` function ka istemal karte hain. Isse hum original string ko modify nahi karte, balki ek naya lower case string banate hain. Chaliye ek simple example ke through samajhte hain ki hum iss function ko kaise use kar sakte hain:

```Rust
let name = String::from("Pooja");
let name_lower = name.to_lowercase();

println!("{}", name_lower); // Outputs "pooja"
```

Yahan humne `to_lowercase()` function ko use kiya hai, jisse `name` string ko lower case mein convert kiya gaya hai aur naya string `name_lower` mein store kiya gaya hai. Fir hum `println!()` function ka istemal karke naya string ko output karte hain.

Agar aapko khud `to_lowercase()` function ka implementation dekhna hai, to aap [Rust standard library](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase) refer kar sakte hain.

## Gehri Jankari

Agar hum iss topic ko aur gehre se samajhna chahte hain, to hume ASCII aur Unicode ke bare mein bhi thoda gyan hona chahiye. ASCII (American Standard Code for Information Interchange) ek character encoding standard hai, jiska istemal primarily English alphabet aur kuch special characters ke representation mein hota hai. ASCII mein pehle 128 characters ke liye 7 bits ka encoding hota hai.

Unicode ek bada character set hai, jisme humare paas 1,114,112 characters ki ek range hoti hai. Iss range mein hum English, Arabic, Chinese, Greek, Hindi, Latin jaise bohot saare languages ko cover kar sakte hain. Isse hume ek unified way mein characters ko represent karne ka mauka milta hai.

Yeh zaroori nahi hai ki sabhi characters ASCII characters ke saath compatible ho. Isliye, agar aap kisi non-ASCII character ko lower case mein convert karenge, to `to_lowercase()` function usse ASCII character ke saath replace kar dega.

## Dekhna Bhi

Agar aapko further details aur examples chahiye, to aap [Rust programming language](https://www.rust-lang.org/) aur [Rust standard library](https://doc.rust-lang.org/std/) refer kar sakte hain.

See Also:
- [Rust String documentation](https://doc.rust-lang.org/std/string/index.html)
- [Rust Unicode documentation](https://doc.rust-lang.org/std/string/struct.String.html#encoding)
- [ASCII vs. Unicode: What's the difference?](https://www.computerhope.com/issues/ch001624.htm)