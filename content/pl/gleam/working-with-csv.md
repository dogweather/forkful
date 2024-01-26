---
title:                "Praca z plikami CSV"
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z plikami CSV (Comma-Separated Values) to zarządzanie danymi w formacie tabelarycznym - prostym i powszechnym. Programiści używają go do importu, eksportu, i przetwarzania danych między różnymi systemami i aplikacjami.

## Jak to zrobić:
W Gleam, operowanie na CSV jest proste. Oto przykład:

```gleam
import gleam/csv
import gleam/io

fn main() {
  let data = "name,age,city\nJohn,30,New York\nAnna,25,Berlin"
  let rows = csv.decode(data)
  
  case rows {
    Ok(values) -> io.print(values)
    Error(err) -> io.print(err)
  }
}
```
Wejście jest ciągiem znaków w formacie CSV, wyjście będzie listą wartości lub błędem.

## Deep Dive
Historia formatu CSV sięga wczesnych lat komputerowych, gdzie prostota formatu pomogła w wymianie danych między różnymi systemami. Alternatywy jak JSON czy XML są bardziej elastyczne, ale też bardziej złożone. Implementacja w Gleam korzysta z podstawowych funkcji języka, by przekształcić tekst w strukturę danych i vice versa.

## Zobacz również
- Oficjalna dokumentacja Gleam na temat CSV: https://hexdocs.pm/gleam_csv/
- Tutorial CSV w Gleam na blogu: [Link do bloga z tutorialem]
- RFC 4180 - formalna specyfikacja CSV: https://tools.ietf.org/html/rfc4180
