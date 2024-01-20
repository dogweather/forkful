---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie daty z napisu to konwersja przedstawienia date formatu tekstowego na strukturę danych łatwiej wykorzystywalną w programie. Programiści przeprowadzają tę operację, żeby można było wygodnie manipulować datami i czasem.

## Jak to zrobić:

Instalujemy zewnętrzną bibliotekę chrono do naszego projektu. Dodaj to do twojego pliku Cargo.toml:

```Rust
[dependencies]
chrono = "0.4"
```

Potem wprowadź poniższy kod w swoim programie:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

let date_string = "2022-01-01 00:01:02";
let parsed_date = NaiveDateTime::parse_from_str(date_string, "%Y-%m-%d %H:%M:%S");

match parsed_date {
    Ok(date) => println!("Data: {}", date),
    Err(err) => println!("Błąd: {}", err),
}
```

Na wyjściu zobaczysz: `Data: 2022-01-01 00:01:02`

## Wgłębna analiza:

Historia parsowania daty z napisu sięga czasów, gdy komputery miały mało przestrzeni na dyskach i często zapisywano daty jako ciągi znaków, by oszczędzić miejsce. Obecnie to jest istotne dla przetwarzania dat i czasu z różną granulacją i formatami.

Alternatywnie, można próbować parsować datę bez zewnętrznych bibliotek, ale jest to trudne ze względu na różne formaty dat i strefy czasowe.

Jeśli chodzi o szczegóły implementacji, metoda parse_from_str używa formatu, który określa jakie elementy i w jakiej kolejności są oczekiwane w napisie. Przykład "%Y-%m-%d %H:%M:%S" oznacza rok, miesiąc, dzień, godziny, minuty i sekundy.

## Zobacz też:

Sprawdź dokumentację chrono tutaj: https://docs.rs/chrono/0.4.0/chrono/

Wypróbuj także inne biblioteki jak datetime: https://crates.io/crates/datetime

Przejrzyj podejścia do formatowania i parsowania daty w Rust tutaj: https://dev.to/pauliuskupinis/handling-datetime-formatting-and-parsing-in-rust-1kbb