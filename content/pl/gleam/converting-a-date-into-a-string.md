---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwertowanie daty na łańcuch znaków pozwala na łatwe i zrozumiałe przedstawienie daty w czytelnej formie. Programiści robią to, aby ułatwić interakcję użytkownika z informacją daty.

## Jak to zrobić:

Przykład konwersji daty na łańcuch znaków:

```Gleam
import gleam/date.{Date}
import gleam/string

pub fn date_to_string(date: Date) -> String {
  string.append(date.year, "-", date.month, "-", date.day)
}
```

Przykładowy wynik:
```Gleam
date_to_string(Date(2021, 12, 9))
``` 
zwróci „2021-12-09”.

## Głębsze zanurzenie

Pomysł konwersji dat do postaci czytelnej dla człowieka pochodzi z czasów, gdy zaczęto komputeryzować zarządzanie danymi. Pomysł jest prosty, ale jest wiele sposobów implementacji.

Jedną z alternatyw jest użycie funkcji formatujących datę:
```Gleam
import gleam/date.{Format, Date}
import gleam/string

pub fn date_to_string_v2(date: Date) -> String {
  string.format(date, Format("YYYY-MM-DD"))
}
```

Szczegóły implementacyjne zależą od konkretnych wymagań. Czasami konwersja na łańcuch znaków jest używana do serializacji dat lub do tworzenia etykiet wyświetlanych użytkownikowi. Ważne jest, aby zawsze sprawdzać, že format wynikowy jest zgodny z oczekiwaniami.

## Zobacz także

[Versja Gleam](https://gleam.run/docs/introduction/): Dokumentacja i przewodnik
[Formatowanie daty](https://gleam.run/docs/reference-manual-introduction/): Szczegółowe informacje o formatowaniu daty
[Formaty daty ISO](https://pl.wikipedia.org/wiki/ISO_8601): Szczegółowe informacje o formatach daty