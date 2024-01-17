---
title:                "वर्तमान तारीख पाना"
html_title:           "Rust: वर्तमान तारीख पाना"
simple_title:         "वर्तमान तारीख पाना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kya Aur Kyu?
"Kya Hum abhi ki tarikh nikal sakte hain? Aur kyu programmers isse karte hain?"

Abhi ki tarikh nikalna ek aam kaam hai jo kayi baar programmers ko karne ko milta hai. Isse hum apne program me sahi tarikh daal sakte hain aur future ke tasks aur events par nazar rakh sakte hain.

## Kaise Karein:
```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now();
let secs = now.duration_since(UNIX_EPOCH).unwrap().as_secs();

println!("Current Date: {}", secs);
```

Is code se hum ek Unix timestamp nikal sakte hain jo current date ko represent karta hai. Hum ```chrono``` library bhi use kar sakte hain jo same functionality provide karta hai.

## Gehri Jankari:
Is kaam ko karne ke liye, hum ```std::time::SystemTime``` aur ```std::time::Duration``` structs ka istemal karte hain. Iske alawa, hum apne desired format me date ko display karne ke liye ```chrono``` aur ```time``` libraries ka istemal kar sakte hain. 

Ek aur tarika hai current date ko get karne ka, jisme hum Internet se time fetch karte hain. Isme hum "http" ya "ntp" libraries ka istemal kar sakte hain lekin ye approach dependability ko kam kar sakta hai.

## Jyada Padhein:
- [Rust Standard Library Documentation for Date and Time](https://doc.rust-lang.org/std/time/index.html)
- [Chrono library](https://crates.io/crates/chrono)
- [Time library](https://crates.io/crates/time)

Itna karne ke baad, aapko current date nikalne me asani hogi! Happy coding!