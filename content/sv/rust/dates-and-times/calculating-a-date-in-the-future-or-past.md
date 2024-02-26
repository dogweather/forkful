---
date: 2024-01-20 17:32:12.519832-07:00
description: "Ber\xE4kning av ett datum i framtiden eller f\xF6rflutet handlar om\
  \ att ta ett givet datum och addera eller subtrahera tid fr\xE5n det. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
lastmod: '2024-02-25T18:49:36.007356-07:00'
model: gpt-4-1106-preview
summary: "Ber\xE4kning av ett datum i framtiden eller f\xF6rflutet handlar om att\
  \ ta ett givet datum och addera eller subtrahera tid fr\xE5n det. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutenheten"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett datum i framtiden eller förflutet handlar om att ta ett givet datum och addera eller subtrahera tid från det. Programmerare gör detta för att hantera tidsbaserade händelser, t.ex. för att beräkna slutdatum för uppgifter eller avgöra tidskillnader.

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
