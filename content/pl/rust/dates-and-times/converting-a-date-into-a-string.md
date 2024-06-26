---
date: 2024-01-20 17:37:37.166560-07:00
description: "How to: (Jak to zrobi\u0107?) Na pocz\u0105tek, zaimportuj `chrono`\
  \ \u2013 popularn\u0105 bibliotek\u0119 do zarz\u0105dzania czasem. Potem u\u017C\
  yj w\u0142a\u015Bciwej metody do konwersji. Poka\u017C\u0119\u2026"
lastmod: '2024-04-05T21:53:36.632931-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) Na pocz\u0105tek, zaimportuj `chrono` \u2013 popularn\u0105\
  \ bibliotek\u0119 do zarz\u0105dzania czasem."
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## How to: (Jak to zrobić?)
Na początek, zaimportuj `chrono` – popularną bibliotekę do zarządzania czasem. Potem użyj właściwej metody do konwersji. Pokażę to na przykładzie:

```rust
extern crate chrono;
use chrono::{DateTime, Utc, Local};

fn main() {
    let now_utc: DateTime<Utc> = Utc::now();
    let now_local: DateTime<Local> = Local::now();
    
    // Formatowanie do String w standardowym formacie RFC 3339
    let utc_string = now_utc.to_rfc3339();
    let local_string = now_local.to_rfc3339();
    
    println!("UTC time: {}", utc_string);
    println!("Local time: {}", local_string);

    // Dostosowanie formatu daty
    let custom_format = now_local.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("Custom local time: {}", custom_format);
}
```
Wyjście z programu pokazuje daty w różnych formatach – zarówno UTC, jak i lokalny.

## Deep Dive (Dogłębna analiza)
Czas w informatyce ma długą historię. Rust używa systemu `chrono` jako de facto standardu do manipulacji czasem. Alternatywą jest wbudowany w standardową bibliotekę moduł `std::time`, ale ten jest mniej elastyczny.

`chrono` oferuje wiele metod do transformaty, włączając w to wsparcie dla stref czasowych. Konwersja daty do stringa odbywa się przez formater, który pozwala na określenie niemal dowolnego formatu wyjściowego.

Co więcej, Rust zapewnia bezpieczeństwo typów przy konwersji, minimalizując ryzyko błędów związanych z formatem dat. To mocno ogranicza możliwe błędy związane z obsługą czasu.

## See Also (Zobacz również)
- Oficjalna dokumentacja `chrono`: https://docs.rs/chrono/
- Rust by Example, rozdział o czasie: https://doc.rust-lang.org/rust-by-example/std_misc/chrono.html
- Tutorial o manipulacji czasem w Rust: https://stevedonovan.github.io/rustifications/2018/09/08/date-time-in-rust.html
