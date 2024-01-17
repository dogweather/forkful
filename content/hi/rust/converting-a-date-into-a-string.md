---
title:                "एक तारीख को स्ट्रिंग में रूपांतरित करना"
html_title:           "Rust: एक तारीख को स्ट्रिंग में रूपांतरित करना"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kya Aur Kyun?
Tareekh ko ek string mein badalna ek prachalit prakriya hai jismein programmers tareekh ko aasani se padhne aur samajhne ke liye ek string mein convert karte hain. Yeh kaam kisi bhi programming language mein kiya ja sakta hai, lekin hum aaj Rust mein is prakriya ki baat karenge. Is process ka upyog alag-alag programming tasks mein date ko manipulate karne ke liye kiya jaata hai.

## Kaise Karein:
Mehmaan, ab baat aati hai ki tareekh ko string mein convert kaise karein. Rust mein, hum is kaam ke liye ```DateTime``` aur ```format``` libraries ka upyog karte hain. Neeche diye gaye code block mein ek simple example hai jo date ko "DD/MM/YYYY" format mein string mein convert karta hai.

```Rust
use std::time::SystemTime;
use chrono::{DateTime, Utc, Datelike};

fn main() {
    let now: DateTime<Utc> = SystemTime::now().into();
    let string_date = format!("{}", now.format("%d/%m/%Y"));
    println!("{}", string_date); // Output: 17/08/2021
}
```

Is code mein humne ```DateTime``` aur ```format``` libraries ko import kiya hai. Phir, hum ```now``` constant variable mein aktarit tareekh ko "DateTime" object ke roop mein store karte hain. Uske baad, hum ```format``` function ka upyog karke tareekh ko "DD/MM/YYYY" format mein string mein convert karte hain. Is code ka output "17/08/2021" hoga.

## Gehri Khurak:
Doston, date ko string mein convert karne ka yeh tarika kaafi asaan aur upyogi hai. Isse humans ke liye bhi date ko padhna aur comprehend karna aasan ho jata hai. Lekin yeh prakriya huee tareekh se judi ek lambi history se judi hai. Pehle, date ko calculate karne ke liye mathematical formulas ka upyog kiya jaata tha. Lekin ab computers ke aane se yeh prakriya kaafi easy aur precise ho gayi hai.

Agar aapki project mein tareekhon ke saath kaam karna hai toh aap ```DateTime``` aur ```format``` libraries ke alawa bhi ```chrono``` library ka upyog kar sakte hain. Yeh ek bahut hi useful library hai jiske ek saath kayi tarah ke date-time manipulation functions available hain.

Iske alawa, aap date ko string mein convert karne ke liye dusre programming languages jaise C++, Java, Python, etc. ka bhi upyog kar sakte hain. Har language mein alag-alag libraries aur methods hote hain, lekin basic concept same rehta hai.

## See Also:
Agar aapko aur deep-dive information chahiye converting a date into a string ke baare mein toh aap neeche diye gaye sources ko refer kar sakte hain:

- [Rust Documentation - Date and Time](https://doc.rust-lang.org/std/time/)
- [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [GeeksforGeeks - Date to String in Rust](https://www.geeksforgeeks.org/date-string-rust/)
- [Stack Overflow - Convert Date to String in Rust](https://stackoverflow.com/questions/30748104/convert-date-to-string-in-rust)