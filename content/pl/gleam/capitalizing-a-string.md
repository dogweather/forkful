---
title:                "Zapisywanie ciągu znaków wielką literą"
html_title:           "Gleam: Zapisywanie ciągu znaków wielką literą"
simple_title:         "Zapisywanie ciągu znaków wielką literą"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego to robimy?

Kapitalizacja ciągu znaków to proces zamiany pierwszej litery na wielką, a pozostałych na małe. Programiści robią to w celu poprawy czytelności kodu oraz konwencji nazewnictwa.

## Jak to zrobić?

Kodując w języku Gleam, wystarczy użyć funkcji `String.capitalize` aby skapitalizować dany ciąg znaków. Przykłady i wyniki można zobaczyć poniżej:

```Gleam
import Gleam.String

let result = String.capitalize("programowanie jest fajne")
// Output -> "Programowanie jest fajne"

let result2 = String.capitalize("ALa ma kota")
// Output -> "Ala ma kota"
```

## Głębsza analiza

Kapitalizacja ciągów znaków była wykorzystywana już w dawnych językach programowania, takich jak BASIC i Pascal. Alternatywą dla funkcji `String.capitalize` w języku Gleam jest użycie metody `capitalize` z pakietu `Unicode` lub napisanie własnej funkcji. Implementacja tej funkcji jest prostym zadaniem i może być przydatna w wielu przypadkach.

## Zobacz także

Dla większej ilości informacji na temat kapitalizacji ciągów znaków w języku Gleam, polecamy przeczytać dokumentację języka lub odwiedzić oficjalną stronę internetową.