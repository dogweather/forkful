---
title:                "Praca z formatem json"
html_title:           "Rust: Praca z formatem json"
simple_title:         "Praca z formatem json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

JSON (JavaScript Object Notation) jest powszechnym formatem do przechowywania i przesyłania danych w postaci tekstu. Programiści używają JSON w celu łatwiejszego przechowywania i przetwarzania danych, szczególnie w przypadku komunikacji między różnymi systemami lub językami programowania.

## Jak:

Przykładowy kod w języku Rust do konwersji danych z JSON na obiekty:

```
use serde_json::{Result, Value};
use std::fs;
 
fn main() -> Result<()> {
    // Wczytanie danych z pliku JSON
    let data = fs::read_to_string("data.json").expect("Nie udało się odczytać pliku.");
 
    // Parsowanie danych do obiektu Value
    let json_data: Value = serde_json::from_str(&data)?;
 
    // Przykładowe wykorzystanie danych
    println!("Imię: {}", json_data["name"]);
    println!("Wiek: {}", json_data["age"]);
 
    Ok(())
}
```
Wynik:

```
Imię: Anna
Wiek: 25
```

## Głębsza analiza:

1. JSON został stworzony w 2001 roku przez Douglasa Crockforda jako prosty format wymiany danych w JavaScript. Obecnie jest powszechnie używany przez wiele języków programowania.

2. Istnieją inne formaty do przechowywania i przetwarzania danych, takie jak XML czy CSV, jednak JSON jest uważany za bardziej czytelny i łatwiejszy w użyciu.

3. Implementacja biblioteki serde_json w języku Rust zapewnia szybkie i wydajne przetwarzanie danych JSON.

## Zobacz też:

- [Dokumentacja serde_json](https://docs.rs/serde_json/latest/serde_json/)
- [Wprowadzenie do JSON](https://www.json.org/json-pl.html)