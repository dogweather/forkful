---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases:
- /pl/rust/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:16:02.639723-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z ciągu znaków to częste zadanie podczas pracy z danymi wprowadzanymi przez użytkownika lub odczytywaniem danych z plików. Polega to na konwersji danych w ciągu znaków na format daty rozpoznawany przez język programowania. W Rust, jest to kluczowe dla operacji na datach, takich jak porównania, arytmetyka czy formatowanie, i zwiększa walidację oraz integralność danych w aplikacjach.

## Jak to zrobić:

### Używając standardowej biblioteki Rusta (`chrono` Crate)
Standardowa biblioteka Rusta bezpośrednio nie obejmuje parsowania daty, ale szeroko używany `chrono` crate jest solidnym rozwiązaniem do manipulacji datą i czasem. Najpierw dodaj `chrono` do swojego `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Następnie użyj `chrono` do sparsowania ciągu znaków daty na obiekt `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Nie udało się sparsować daty");

    println!("Sparsowana data: {}", date);
}

// Przykładowe wyjście:
// Sparsowana data: 2023-04-01
```

### Używając zaawansowanej obsługi daty-czasu Rusta (`time` Crate)
Dla bardziej zaawansowanej obsługi daty-czasu, w tym bardziej ergonomicznego parsowania, rozważ użycie crate'a `time`. Najpierw dołącz go do swojego `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Następnie sparsuj ciąg znaków daty używając typu `Date` i `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Nie udało się sparsować daty i czasu");

    println!("Sparsowany datetime: {}", parsed_date);
}

// Przykładowe wyjście:
// Sparsowany datetime: 2023-04-01 12:34:56
```

Oba przykłady pokazują, jak Rust przy pomocy zewnętrznych crate'ów ułatwia parsowanie ciągów znaków daty na manipulowalne obiekty daty, co czyni go potężnym narzędziem do tworzenia oprogramowania pracującego z danymi czasowymi.
