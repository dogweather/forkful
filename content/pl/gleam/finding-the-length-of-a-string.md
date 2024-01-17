---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Gleam: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Znalezienie długości łańcucha znakowego to podstawowa umiejętność programistów. Polega ona na określeniu ilości znaków w danym ciągu. Jest to często potrzebne do przetwarzania danych lub tworzenia warunków logicznych w programach.

## Jak to zrobić:

### Gleam:

```gleam
let string = "Hello, world!"
 
let length = String.length(string)
 
// length = 13
```

## Głębsze Wprowadzenie

### Kontekst historyczny:

Pierwszym językiem programowania, który wprowadził funkcję dla znajdowania długości znaków, był ALGOL 60. Następnie stało się to standardem w językach programowania.

### Alternatywy:

Innym sposobem na znalezienie długości łańcucha znakowego jest użycie pętli i inkrementowania licznika przy każdym przejściu przez kolejny znak. Jednak w języku Gleam funkcja String.length jest szybsza i bardziej wydajna.

### Szczegóły implementacji:

Funkcja String.length zwraca liczbę całkowitą, która reprezentuje długość łańcucha znakowego. Jest ona zaimplementowana w języku Erlang, na którym opiera się Gleam.

## Zobacz też:

- [Dokumentacja Gleam o funkcji String.length](https://gleam.run/documentation/)