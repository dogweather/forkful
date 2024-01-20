---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:16:29.357469-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pobieranie aktualnej daty to proces, w którym Twój program ustala, jaka jest obecnie data według systemowego zegara komputera. Programiści robią to zwykle, aby zapisywać logi, ustalać timery, czy też w celach biznesowych, jak np. obliczanie daty zapadalności płatności.

## Jak to zrobić:
Używamy crate `chrono` do łatwego uzyskania daty. Najpierw dodaj go do swojego `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Potem pobierz datę jak tutaj:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let current_date = Local::now();
    println!("Dzisiejsza data: {}", current_date.format("%Y-%m-%d").to_string());
}
```

Przykładowe wyjście może wyglądać tak:

```
Dzisiejsza data: 2023-04-05
```

## Deep Dive
W Rust, operacje na datach nie są wbudowane w język. Crate `chrono` jest de facto standardem do obsługi czasu i daty. Jest oparty na popularnej bibliotece C++ `boost::date_time`. Alternatywnie, od wersji Rust 1.39.0 można by użyć typów z modułu `std::time`, ale `chrono` oferuje więcej funkcji i jest łatwiejszy w użyciu.

`chrono` obsługuje zarówno czas systemowy, jak i czas UTC. Aby uzyskać bieżącą datę, używa się funkcji jak `Local::now()`, która daje `DateTime` z lokalną strefą czasową, albo `Utc::now()` dla czasu uniwersalnego.

## Zobacz także
- Oficjalna dokumentacja crate `chrono`: https://docs.rs/chrono/
- Rust by Example, rozdział o czasie: https://doc.rust-lang.org/rust-by-example/std_misc/time.html
- The Rust Programming Language, rozdział o obsłudze błędów (czas i data mogą też generować błędy!): https://doc.rust-lang.org/book/ch09-00-error-handling.html