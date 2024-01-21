---
title:                "Konwersja daty na łańcuch znaków"
date:                  2024-01-20T17:36:25.203013-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja daty na łańcuch znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest i po co to robimy? Konwersja daty na łańcuch znaków pozwala na zapisywanie i wyświetlanie dat w zrozumiałej formie. Programiści używają tego, by dane czasowe były czytelne dla użytkowników.

## How to:
Gleam ma wbudowane funkcje do pracy z datami i czasem. Oto przykład konwersji daty na łańcuch znaków:

```gleam
import gleam/calendar.{Date}
import gleam/datetime

fn main() {
  let date = Date(year: 2023, month: 4, day: 10)
  let date_string = datetime.to_iso8601(date)
  date_string
}
```

Sample output:

```
"2023-04-10"
```

## Deep Dive
Historia konwersji dat sięga czasów przed komputerowych, gdzie data jako łańcuch znaków była zapisywana na piśmie. W świecie programowania, konwersja daty do łańcucha znaków była jedną z pierwszych operacji formatujących, które zyskały na znaczeniu. Alternatywą jest stosowanie timestampów, jednak są one trudniejsze w odczycie dla człowieka. Implementacja konwersji w Gleam jest bezpośrednia i korzysta z funkcji `to_iso8601`, która zwraca datę w formacie ISO 8601, akceptowanym międzynarodowym standardzie reprezentowania dat i czasów.

## See Also
Więcej na temat dat i czasów w Gleam znajdziesz w oficjalnej dokumentacji:
- Gleam calendar library: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Gleam datetime functions: https://hexdocs.pm/gleam_stdlib/gleam/datetime/