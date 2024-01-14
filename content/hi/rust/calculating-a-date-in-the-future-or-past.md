---
title:                "Rust: भविष्य या भूतकाल में एक तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyu:

 Agar aap ek programmer hai aur aapko future ya past mein kisi specific date ko calculate karna hai to Rust programming language aapke liye ek accha option ho sakta hai. Yeh ek efficient, safe, aur fast language hai jo aapko accurate results provide karta hai.

## Kaise Karein:

Rust mein date calculate karne ke liye aapko datetime library ka use karna hoga. Sabse pehle, aapko `chrono` crate ko apne project mein add karna hoga. Iske baad, aapko ek `DateTime` object banana hoga jis mein aap apni desired date ko define kar sakte hai. Yeh object year, month, day, hour, minute, aur second ko represent karta hai. Uske baad, aap `DateTime` object mein se future ya past mein kitne days add ya subtract karna hai wo specify kar sakte hai.

```Rust
use chrono::{DateTime, Local, Duration};
// Add 10 days to today's date
let now = Local::now();
let future_date = now + Duration::days(10); 
// Subtract 5 days from today's date
let past_date = now - Duration::days(5); 
println!("Future date: {}", future_date.format("%Y-%m-%d"));
println!("Past date: {}", past_date.format("%Y-%m-%d"));
```

Output:

```
Future date: 2020-08-29
Past date: 2020-08-14
```

## Gehri Jankari:

Rust mein date calculate karne ka process kaafi simple hai aur datetime crate aapko ismein help karta hai. Iss crate mein aapko `DateTime` object ke alawa bhi kai aur standard date and time formats milte hai jaise ISO 8601 aur RFC 3339. Aap apne project ke requirements ke hisaab se in formats ka use kar sakte hai.

Iss process ke alawa aap Rust ki official documentation aur online tutorials se bhi aur gehri jankari prapt kar sakte hai.

## Dekhiye Bhi:

- [Rust Programming Language](https://www.rust-lang.org/)
- [Chrono Crate Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Tutorials on YouTube](https://www.youtube.com/playlist?list=PLV176MJSI2DXC3ZnpIOELeg0OukvLwL97)