---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:38:30.256356-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Parsowanie daty z ciągu tekstowego to proces konwertowania tekstu na strukturę danych reprezentującą datę. Programiści robią to, aby umożliwić komputerom rozumienie i manipulowanie datami na podstawie danych wejściowych z różnych źródeł, np. formularzy internetowych.

## Jak to zrobić:
```Rust
use chrono::{NaiveDate, ParseError};

fn parse_date_from_string(date_str: &str) -> Result<NaiveDate, ParseError> {
    NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
}

fn main() {
    let date_input = "2023-04-05";
    match parse_date_from_string(date_input) {
        Ok(date) => println!("Parsed date: {}", date),
        Err(e) => println!("Error parsing date: {}", e),
    }
}
```
Przykładowy wynik:
```
Parsed date: 2023-04-05
```

## Więcej szczegółów:
Historia parsowania dat w informatyce jest ściśle związana z potrzebą standardizacji i lokalizacji formatu dat. Biblioteki takie jak `chrono` w Rust zapewniają elastyczne i wydajne narzędzia do parsowania dat w różnych formatach. Alternatywy obejmują używanie standardowej biblioteki Rust `time`, ale `chrono` jest szerszy w zastosowaniach i bardziej popularny. Implementacja parsowania opiera się na precyzyjnym określeniu wzorców, którymi są formaty dat i godzin (np. `%Y-%m-%d` dla `YYYY-MM-DD`).

## Zobacz również:
- Dokumentacja Chrono: https://docs.rs/chrono/
- Rust API Guidelines o czasie i datach: https://rust-lang.github.io/api-guidelines/about.html
- Klub Rusta (Polska społeczność Rust): https://rust-lang.pl/
