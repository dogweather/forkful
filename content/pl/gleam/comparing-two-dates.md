---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Porównywanie dwóch dat to proces sprawdzania, która z nich jest wcześniejsza lub późniejsza. Programiści robią to, aby zrozumieć różnicę czasu pomiędzy dwoma momentami lub uporządkować dane według czasu.

## Jak to zrobić:

Za pomocą biblioteki `gleam/date` w Gleam, porównywanie dat jest proste. Tutaj jest jak to można zrobić:

```gleam
import gleam/date.{from_timestamp, from_tuple}

let date_one = from_tuple(~year=2020, ~month=1, ~day=1)
let date_two = from_timestamp(1609459200)

date.compare(date_one, date_two)
```

To zwróci `Gt` jeśli pierwsza data jest późniejsza, `Lt` jeśli jest wcześniejsza, lub `Eq` jeśli są równe.

## Głębiej

Porównywanie dat jest starym zagadnieniem w komputerach - początkowo to było wyzwanie, ze względu na różnice w sąsiednich strefach czasowych. W związku z tym powstało wiele alternatyw jak Unix Timestamp, który uniezależnił datę od lokalnej strefy czasowej.

Jednak, w nowoczesnym kodowaniu Gleam to jest bardziej proste. `from_tuple` pozwala na tworzenie dat z uporządkowanych tuples, a `from_timestamp` pozwala na tworzenie dat z sekund od 1 stycznia 1970 roku.

Kiedy daty są dostępne, możemy je porównać za pomocą metody `compare`. Faktycznie, `compare` tylko sprawdza różnice między komponentami daty, ale dla nas to jest ukryte przez bibliotekę `gleam/date`.

## Zobacz także:

- Dokumentacja Gleam na temat dat: https://hexdocs.pm/gleam_stdlib/date.html
- Artykuł na temat historii czasu Unix: https://www.wikiwand.com/pl/Unix_Time
- Artykuł na temat stref czasowych: https://www.wikiwand.com/pl/Strefa_czasowa