---
date: 2024-01-20 17:32:12.519832-07:00
description: "Hur g\xF6r man: F\xF6r att hantera datum och tid i Rust anv\xE4nder\
  \ vi `chrono`-biblioteket. H\xE4r \xE4r en enkel guide."
lastmod: '2024-03-13T22:44:37.712706-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att hantera datum och tid i Rust anv\xE4nder vi `chrono`-biblioteket."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
weight: 26
---

## Hur gör man:
För att hantera datum och tid i Rust använder vi `chrono`-biblioteket. Här är en enkel guide:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc, Duration};

fn main() {
    // Nuvarande tidpunkt
    let now: DateTime<Utc> = Utc::now();
    println!("Nu: {}", now);

    // Beräkna ett datum 30 dagar i framtiden
    let future_date = now + Duration::days(30);
    println!("Framtid: {}", future_date);

    // Beräkna ett datum 30 dagar i det förflutna
    let past_date = now - Duration::days(30);
    println!("Förflutet: {}", past_date);
}
```
Sample output:
```
Nu: 2023-04-12T15:30:45Z
Framtid: 2023-05-12T15:30:45Z
Förflutet: 2023-03-13T15:30:45Z
```

## Fördjupning
Tidigare hanterades tider i standardbiblioteket `std::time`, men det gav begränsad funktionalitet. `chrono` är nu det mest populära valet för Rust-utvecklare när det gäller datum och tid. Det finns alternativ som `time`-biblioteket, men `chrono` erbjuder en rik uppsättning funktioner och är väl etablerat.

När man implementerar datumberäkningar i `chrono`, bör man tänka på tidszoner och eventuell omställning till/sommar- och vintertid. `chrono` hanterar detta smidigt, vilket gör det till ett robust val för datum- och tidshantering.

## Se även
- Chrono documentation: https://docs.rs/chrono/
- Rust documentation for `std::time`: https://doc.rust-lang.org/std/time/
- Time library: https://docs.rs/time/
