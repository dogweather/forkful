---
title:                "नियमित अभिव्यक्तियों का उपयोग करना"
html_title:           "Rust: नियमित अभिव्यक्तियों का उपयोग करना"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Regular Expressions in Rust: 

## Kya & Kyun? 
Regular Expressions, ya 'Regex' kehte hain, ek powerful tool hai jiska istemaal programmers log text ko manipulate karne ke liye karte hain. Ye text ko search, replace aur validate karne mein madad karta hai. Aksar programmers 'Regex' ko complex string patterns ko match karne ke liye use karte hain, jaise ki email addresses, phone numbers, aur passwords.

## Kaise karein: 
```Rust
let regex = regex::Regex::new(r"[a-zA-Z]+").unwrap(); // ek naya regex banayein
let text = "Hello, World!"; // text mein se pattern match karein
if regex.is_match(text) { 
    println!("Pattern found!"); // agar pattern mil gaya toh print karein
}
```
Sample output: Pattern found! 

## Gehraai mein jayein: 
Perl programming language mein 'Regex' ka concept pehli baar 1986 mein implement kiya gaya tha. Lekin aaj kal ye major programming languages, jaise ki Rust, Python, aur Java mein bhi available hai. Agar aapke paas ek complex string pattern hai jo traditional methods se match nahi ho raha hai, toh 'Regex' aapke kaam ka saathi ho sakta hai. Iske alawa, aap 'Regex' ko text mining, data validation, aur data cleaning mein bhi istemaal kar sakte hain. 

## Aur jaaniye: 
Agar aapko 'Regex' ke baare mein aur jaanna hai toh, [Official Rust Documentation](https://doc.rust-lang.org/std/regex/), [Regular-Expressions.info](https://www.regular-expressions.info/), aur [Regex Guru](https://www.regular-expressions.info/tutorial.html) aapke liye helpful resources ho sakte hain.