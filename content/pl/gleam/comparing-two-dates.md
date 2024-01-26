---
title:                "Porównywanie dwóch dat"
date:                  2024-01-20T17:33:03.650408-07:00
model:                 gpt-4-1106-preview
simple_title:         "Porównywanie dwóch dat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Porównywanie dwóch dat pozwala ustalić, która jest wcześniejsza, późniejsza czy identyczna. Programiści robią to na przykład do weryfikacji ważności terminów, sortowania wydarzeń czy obliczeń związanych z czasem.

## How to: (Jak to zrobić:)
```gleam
import gleam/calendar.{Date}
import gleam/int

fn main() {
  let date1 = Date(2023, 3, 14)
  let date2 = Date(2024, 1, 18)
  
  int.compare(date1.to_iso_days(), date2.to_iso_days())
}
```

Sample output (Przykładowe wyjście):
```
-1 // date1 jest wcześniejsza niż date2
```

## Deep Dive (Dogłębna analiza)
Gleam, funkcjonalny język z rodziny ML, używa modułu `calendar` do pracy z datami. W przedstawionym przykładzie, dwie daty są porównywane poprzez konwersję do dni w formacie ISO i użycie funkcji `int.compare`. Porównanie dat jest fundamentem w systemach rezerwacji, logach serwerów i aplikacjach zarządzania czasem. Alternatywy to rozwiązania zewnętrzne, jak biblioteka `chrono` w języku Rust czy klasa `DateTime` w .NET. Gleam zapewnia jednak bezpieczne operacje na datach dzięki statycznemu typowaniu i pattern matching.

## See Also (Zobacz także)
- Oficjalna dokumentacja Gleam: [https://gleam.run](https://gleam.run)
