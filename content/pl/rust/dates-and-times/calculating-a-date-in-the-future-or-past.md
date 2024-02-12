---
title:                "Obliczanie daty w przyszłości lub przeszłości"
aliases:
- /pl/rust/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:10.522439-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? - "Co i dlaczego?"
Obliczanie dat w przyszłości czy przeszłości to ustalenie dnia przed lub po określonym czasie. Programiści robią to, aby zarządzać harmonogramami, okresami ważności czy interwałami czasowymi.

## How to: - "Jak to zrobić:"
W Rust korzystamy z crate'a `chrono` do łatwego obliczania dat. 

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("Aktualna data i czas: {}", now);

    let two_weeks = Duration::weeks(2);
    let future_date = now + two_weeks;
    println!("Data za dwa tygodnie: {}", future_date);

    let past_date = now - two_weeks;
    println!("Data sprzed dwóch tygodni: {}", past_date);
}
```

Przykładowe wyjście:

```
Aktualna data i czas: 2023-04-04T15:30:00Z
Data za dwa tygodnie: 2023-04-18T15:30:00Z
Data sprzed dwóch tygodni: 2023-03-21T15:30:00Z
```

## Deep Dive - "Dogłębna analiza"
W przeszłości, obliczanie dat było bardziej skomplikowane przez braki narzędzi. Crate `chrono` ułatwia pracę z datami w Rust, obsługując różnice czasowe i strefy czasowe. Alternatywą jest używanie standardowej biblioteki Rust z `std::time`, ale jest mniej wygodna dla operacji na datach. `chrono` pozwala na dokładne i elastyczne manipulowane czasem, uwzględniając przeciągniki jak lata przestępne.

## See Also - "Zobacz także"
- Dokumentacja `chrono`: https://docs.rs/chrono/
- Książka "Rust Programming by Example": https://www.rust-lang.org/learn/books
- Forum dyskusyjne Rust użytkowników: https://users.rust-lang.org/
