---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:36:23.747994-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu znaków pozwala na konwertowanie tekstowych reprezentacji dat do formatu, którym program może łatwo zarządzać. Programiści parsują daty, aby umożliwić operacje takie jak porównanie dat, przechowywanie w bazach danych czy formatowanie do różnych stref czasowych.

## Jak to zrobić:
Gleam jeszcze nie posiada wbudowanej obsługi dla dat, więc będziemy potrzebować zewnętrzną bibliotekę, jak `gleam_datetime`. Aby zainstalować, dodaj jej zależności do `rebar.config` i uruchom `rebar3 deps`.

```gleam
import gleam/datetime
import gleam/int
import gleam/result

pub fn parse_date(date_string: String) -> result.Result(datetime.DateTime, tuple(String, int)) {
  datetime.parse_iso8601(date_string)
}

// Przykładowe użycie
pub fn main() {
  let example_date = "2021-04-12T23:20:50.52Z"
  let parsed_date = parse_date(example_date)
  
  case parsed_date {
    Ok(date) -> datetime.to_string(date)
    Error(_) -> "Couldn't parse the date"
  }
}
```

Output przykładowego użycia może wyglądać tak: `"2021-04-12T23:20:50Z"`

## Głębsze spojrzenie
Historia parsowania dat sięga początków programowania, kiedy to konwersja danych z jednego typu na inny była kluczowa dla obliczeń i rejestrowania czasu. Alternatywy dla ręcznego parsowania obejmują użycie standardowych bibliotek oferowanych przez języki programowania lub dedykowane usługi API. Implementacja w Gleam wykorzystuje standard ISO 8601, który jest międzynarodowym standardem notacji dat i czasu, ułatwiającym interoperacyjność pomiędzy systemami.

## Zobacz również
- [ISO 8601 Wikipedia](https://pl.wikipedia.org/wiki/ISO_8601)
- [Gleam oficjalna strona](https://gleam.run/)
