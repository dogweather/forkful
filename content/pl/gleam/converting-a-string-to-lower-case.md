---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Gleam: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Konwersja tekstu na małe litery to proces zmiany dużych liter w tekście na odpowiadające im małe litery. Programiści stosują tę technikę w celu ujednolicenia danych i ułatwienia porównywania lub wyszukiwania tekstu.

## Jak to zrobić:
```Gleam
let str = "Cześć, Świat!"
let lower_str = String.to_lower(str) 
```

Przykładowy wynik: "cześć, świat!"

## Głębszy Wgląd:
Konwersja na małe litery jest powszechną techniką w programowaniu, a jej korzenie sięgają czasów maszynowych, kiedy to nie istniały jeszcze duże litery. Programiści mogą także użyć metody ```.map()``` aby dokonać konwersji jednocześnie na wiele elementów tekstu.

## Zobacz także:
- Dokumentacja Gleam: https://gleam.run/
- Wprowadzenie do podstaw programowania: https://kobietydokodu.pl/
- Materiały dla początkujących programistów: https://naukacode.pl/