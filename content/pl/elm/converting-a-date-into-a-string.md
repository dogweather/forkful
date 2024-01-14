---
title:                "Elm: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamiana daty na ciąg znaków może wydawać się prosta, jednak w programowaniu może być przydatna w wielu sytuacjach. Na przykład, gdy chcemy wyświetlić datę użytkownikowi w czytelny sposób lub zapisać ją w odpowiednim formacie do bazy danych.

## Jak to zrobić

```elm
import Date exposing (Date)
import Date.Format as Format

-- Przykładowa data
exampleDate = Date.fromParts 2021 12 31

-- Zamiana na ciąg znaków w formacie "DD.MM.YYYY"
Format.format "DD.MM.YYYY" exampleDate
-- Wynik: "31.12.2021"

-- Zamiana na ciąg znaków w formacie "MM/DD/YYYY"
Format.format "MM/DD/YYYY" exampleDate
-- Wynik: "12/31/2021"
```

Możemy wykorzystać funkcję `Date.fromParts` aby utworzyć datę z wybranych części (rok, miesiąc, dzień), a następnie użyć funkcji `Format.format` aby określić pożądany format wynikowego ciągu znaków. 

## Głębsza analiza

W Elm, formatowanie daty jest możliwe dzięki modułowi `Date.Format` oraz funkcji `format`, która przyjmuje dwie wartości - format i obiekt daty. Format jest ciągiem znaków zawierającym symbole reprezentujące różne części daty (np. DD - dzień, MM - miesiąc, YYYY - rok), natomiast obiekt daty jest tworzony przy użyciu funkcji `Date.fromParts`, która przyjmuje rok, miesiąc i dzień jako argumenty.

Istnieje wiele różnych formatów daty, dlatego warto zapoznać się z dokumentacją dotyczącą `Date.Format` aby poznać wszystkie dostępne symbole i możliwości formatowania daty.

## Zobacz też
- Dokumentacja `Date.Format`: https://package.elm-lang.org/packages/elm-lang/core/latest/Date-Format
- Dokumentacja `Date`: https://package.elm-lang.org/packages/elm-lang/core/latest/Date