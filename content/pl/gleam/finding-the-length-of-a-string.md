---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:23.940701-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Znalezienie długości ciągu znaków to określenie ilości elementów w tekście. Programiści robią to, aby zarządzać danymi tekstowymi – sprawdzają miejsce czy na przykład wyświetli się poprawnie.

## How to: (Jak to zrobić:)
W Gleam używamy funkcji `String.length` do znalezienia długości stringa. Oto przykład:

```gleam
import gleam/io

pub fn main() {
  let my_string = "Cześć, Gleam!"
  let length = String.length(my_string)
  io.println(length) // Wypisze "13"
}
```

## Deep Dive (Głębsze spojrzenie)
Gleam to nowoczesny język, który stara się unikać pułapek swoich poprzedników. Funkcja `String.length` jest prostym narzędziem, ale ważnym w operowaniu na danych tekstowych. W innych językach, jak JavaScript, obliczanie długości stringa może być nieintuicyjne ze względu na Unicode. Gleam obsługuje Unicode bez problemów, więc `String.length` zwraca faktyczną liczbę znaków w stringu.

Alternatywy obejmują iterację po znakach lub konwersję na listę, co jednak jest mniej wydajne. Implementacja funkcji `String.length` w Gleam jest zoptymalizowana dla wydajności i dokładności.

## See Also (Zobacz także)
- Oficjalna dokumentacja Gleam dla `String` modułu: https://hexdocs.pm/gleam_stdlib/gleam/String/
- Unicode w programowaniu: https://unicode.org/reports/tr15/
- Porównanie operacji na stringach w różnych językach programowania: https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Longest_common_substring#Compare_with_other_languages
