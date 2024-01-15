---
title:                "दो तारीखों की तुलना करना"
html_title:           "Rust: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyon

Kisi ne kabhi aap se puchha hai ki kis date se apki shaadi hui thi? Ya fir aapne kabhi socha hai ki dono tarikhon ke beech ka samay kitna antar hai? Is prashn ka samadhan karne ke liye, aapko do tarikhon ko tulit karna hoga. Is artikel mein hum is prashn par gahraai se baat karenge aur saath hi dikhayenge ki Date (Tarikh) aapas mein kaise tulit ki ja sakti hai, Rust programming language ka upyog karke.

## Kaise

Tarikhon ko tulit karne ke liye, hum `chrono` library ka upyog karenge jo Rust standard library mein shamil hai. Is library mein `DateTime` aur `Duration` ke data types par kaam kiya ja sakta hai. Neeche diye gaye code blocks mein dikhaya gaya hai ki kaise hum do tarikhon ko rikod kar sakte hain aur unke beech ka samay kaise nikal sakte hain.

```Rust
// Tarikhon ko rikod (parse) karne ka udaharan
let first_date = "1998-09-12".parse::<DateTime<Utc>>().unwrap();
let second_date = "2021-04-25".parse::<DateTime<Utc>>().unwrap();

// Dono tarikhon ke beech ka antar nikalne ka udaharan
let time_diff = second_date - first_date;
println!("Date ke beech ka antar: {}", time_diff);
```

Is tarah se hum `chrono` library ka upyog karke do tarikhon ko rikod kar sakte hain aur unke beech ka antar nikal sakte hain.

## Gahraai Mein

Do tarikhon ko tulit karte samay, humein dhyan dena hoga ki dono tarikhon ko ek hi timezone mein convert karna hoga. Yeh ek common mistake hai jo aksar log karte hain aur is se galat samay ka result prapt ho sakta hai. Iske alawa, hum `Duration` ka upyog karke do tarikhon ke beech ka samay nikal sakte hain aur iska result milliseconds, seconds, minutes, hours, days, weeks ya fir years mein prapt kar sakte hain.

## Dekhein Bhi

- [Rust `chrono` library documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Video tutorial: How to compare dates in Rust using the `chrono` library](https://www.youtube.com/watch?v=gv_xGjVaKes)

# Dekhein Bhi (See Also)

Is prakar se, humne dekha ki do tarikhon ko tulit karne ka upyog kaise kar sakte hain. Is prashn ka samadhan karne ke liye, humne `chrono` library ka upyog kiya jo Rust standard library mein hi upalabdh hai. Hum umeed karte hain ki yeh artikel aapke liye upyogi sabit hoga. Aise hi aur informative articles ke liye hamari website (*add website link here*) par visit karte rahein. Dhanyavaad!