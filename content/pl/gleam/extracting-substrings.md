---
title:                "Ekstrakcja podciągów"
html_title:           "Gleam: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

Ekstrakcja podciągów to proces pobierania części ciągu znaków z głównego ciągu. Programiści często stosują to w celu wydobycia określonych informacji lub danych z dłuższego ciągu. Jest to szczególnie przydatne w przypadku parsowania danych lub manipulacji tekstami.

## Jak to zrobić:

Gleam posiada niezawodne i proste w użyciu funkcje do wyodrębniania podciągów. Przykładowe użycie wygląda następująco:

```Gleam
let text = "To jest przykładowy tekst."
let substring = String.slice(start=11, end=19, text)
```
W powyższym przykładzie pobieramy podciąg z oryginalnego tekstu, zaczynając od 11 znaku i kończąc na 19 znaku. Wynik zostaje przypisany do zmiennej "substring" i będzie to "przykładowy".

Można również wyodrębnić podciąg od wybranego indeksu do końca ciągu, używając tylko jednego argumentu w funkcji "String.slice". Na przykład:

```Gleam
let text = "To jest inny przykładowy tekst."
let substring = String.slice(start=5, text)
```

Wynik będzie taki sam jak w poprzednim przykładzie, tylko tym razem cały ciąg od 5 znaku zostanie wybrany.

## Głębsze zanurzenie:

Ekstrakcja podciągów jest częstym zadaniem w programowaniu, ponieważ pozwala na łatwe manipulowanie tekstami i wydobycie potrzebnych informacji. Alternatywnym sposobem na to jest użycie metody "split", która dzieli ciąg na części według określonego separatora.

Implementacja funkcji "String.slice" w Gleam jest bardzo wydajna i korzysta z metody "sub" dostępnej w wielu językach programowania.

## Zobacz także:

- Dokumentacja Gleam dotycząca funkcji String: https://gleam.run/module/stdlib/String.html
- Wideo tutorial o ekstrakcji podciągów w Gleam: https://www.youtube.com/watch?v=94d7fPfM3Dc