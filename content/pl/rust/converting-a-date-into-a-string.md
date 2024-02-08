---
title:                "Konwersja daty na łańcuch znaków"
aliases:
- pl/rust/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:37.166560-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Konwersja daty do ciągu znaków pozwala zaprezentować informacje o czasie w zrozumiałym formacie. Programiści robią to, by ułatwić użytkownikom odczytanie daty oraz umożliwić zapisanie daty w bazie danych czy pliku.

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
