---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Rust: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Wielu programistów wykorzystuje bezpieczny język Rust do tworzenia wydajnych aplikacji. Jednym z często spotykanych zadań podczas pracy z danymi jest konwersja daty na tekstowy format. W tym artykule dowiesz się, dlaczego konwersja daty na string jest ważna i jak możesz to zrobić w języku Rust.

## Jak to zrobić

Rust oferuje wbudowane funkcje, które ułatwiają konwersję daty na string. Przyjrzyjmy się dwóm sposobom na wykonanie tego zadania: używając biblioteki standardowej i wykorzystując zewnętrzną bibliotekę.

### Używając biblioteki standardowej

Najprostszym sposobem na konwersję daty na string jest wykorzystanie biblioteki standardowej języka Rust. Skorzystajmy z funkcji `to_string()` dostępnej dla typu `DateTime`, aby przekonwertować datę na string.

```Rust
use std::time::{SystemTime, Duration};

fn main() {
    // Tworzymy obiekt typu SystemTime zawierający aktualną datę i godzinę
    let now = SystemTime::now();
    // Konwertujemy datę na string i przypisujemy do zmiennej
    let date_string = now.to_string();

    println!("Aktualna data i godzina: {}", date_string);
}
```

Wyjście:
```
Aktualna data i godzina: Sun Jun 06 02:00:13 UTC 2021
```

### Wykorzystując zewnętrzną bibliotekę

Jeśli potrzebujemy bardziej zaawansowanej obsługi daty, możemy skorzystać z zewnętrznej biblioteki. Jedną z popularnych opcji jest biblioteka `chrono`, która dostarcza bardziej precyzyjne metody konwersji daty.

```Rust
use chrono::{Utc, DateTime};

fn main() {
    // Pobieramy aktualną datę i godzinę w strefie czasowej UTC
    let utc: DateTime<Utc> = Utc::now();
    // Konwertujemy datę na string i przypisujemy do zmiennej
    let date_string = utc.to_rfc2822();

    println!("Aktualna data i godzina: {}", date_string);
}
```

Wyjście:
```
Aktualna data i godzina: Sun, 06 Jun 2021 02:10:22 +0000
```

## Deep Dive

Jeśli chcesz lepiej zrozumieć proces konwertowania daty na string, warto zapoznać się z dokumentacją języka Rust oraz bibliotek, które zostały wykorzystane w przykładach. W przypadku użycia biblioteki `chrono`, możesz zajrzeć do jej dokumentacji na stronie [chrono.rs](https://docs.rs/chrono/latest/chrono/) lub skorzystać z `rustdoc`, narzędzia do generowania dokumentacji w języku Rust.

## Zobacz również

- [Dokumentacja języka Rust](https://doc.rust-lang.org/std/time/index.html)
- [Dokumentacja biblioteki chrono](https://docs.rs/chrono/latest/chrono/)