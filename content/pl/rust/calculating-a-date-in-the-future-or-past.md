---
title:                "Rust: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego?

Obliczanie daty w przyszłości lub w przeszłości może być niezwykle przydatne w codziennej pracy programisty. Możemy wykorzystać to do planowania działań lub tworzenia aplikacji, które wymagają danych z odległych dat.

# Jak to zrobić?

```Rust
use chrono::{NaiveDate, Duration};

fn main() {
    // Tworzona jest obecna data za pomocą Struktury NaiveDateTime
    let current_date = NaiveDate::from_ymd(2021, 10, 10);

    // Obliczanie daty w przyszłości za pomocą metody `add(Duration)`
    let future_date = current_date.add(Duration::days(30));

    // Obliczanie daty w przeszłości za pomocą metody `sub(Duration)`
    let past_date = current_date.sub(Duration::days(30));

    // Wyświetlenie wyników
    println!("Obecna data: {}", current_date);
    println!("Data w przyszłości: {}", future_date);
    println!("Data w przeszłości: {}", past_date);
}
```

```
Obecna data: 2021-10-10
Data w przyszłości: 2021-11-09
Data w przeszłości: 2021-09-10
```

# Głębsze zanurzenie

W języku Rust możemy wykorzystać bibliotekę `chrono`, która zapewnia nam wiele przydatnych narzędzi do zarządzania datami i godzinami. Metoda `add()` oraz `sub()` umożliwiają nam proste i intuicyjne obliczanie dat w przyszłości lub przeszłości za pomocą określonej liczby dni, miesięcy lub lat. Możemy również wykorzystać bardziej zaawansowane metody, takie jak `add_signed()` lub `subtract_signed()`, aby uwzględnić zmiany w strefach czasowych. Biblioteka `chrono` jest również w stanie obsłużyć daty z dokładnością do milisekund oraz wykonywać operacje matematyczne na nich.

# Zobacz również

- Dokumentacja biblioteki `chrono`: https://docs.rs/chrono/
- Przykładowe rozwiązania wykorzystujące obliczanie dat w przyszłości i przeszłości: https://blog.jeckel.se/2017/06/rust-chrono-date-and-time-calculations/
- "Zarządzanie dniami, godzinami i strefami czasowymi w języku Rust": https://www.paddingleft.com/articles/2019/03/31/managing-dates-time-zones-rust.html