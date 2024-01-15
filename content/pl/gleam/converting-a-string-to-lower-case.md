---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Gleam: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Większość aplikacji internetowych i narzędzi, które korzystają z tekstowych danych, muszą operować na znakach w jednolitym formacie. Używanie małych liter jest często wymagane do poprawnego działania programów, dlatego w tym artykule pokazujemy, jak wykonać proste przekształcenie - zmianę wszystkich liter na małe.

## Jak to zrobić

Przekształcenie stringu na małe litery jest bardzo proste w języku Gleam. Wystarczy wywołać funkcję `String.to_lower` i przekazać jako argument żądany tekst. Poniżej przedstawiamy przykład:
```Gleam
let tekst = "Witaj Świecie!"
let wynik = String.to_lower(tekst)

assert wynik == "witaj światem!"
```

Pamiętaj, że zmiana litery na małą odbywa się według standardu Unicode, więc nie musisz martwić się o różnice między literami polskimi a angielskimi.

## Głębsza analiza

W języku Gleam, stringi są traktowane jako kolekcja znaków, dlatego wykorzystujemy funkcję `String.map` do zmiany każdego znaku na małą literę. W praktyce może to wyglądać tak:
```Gleam
fn map_lower(char: String.Char) {
  String.to_lower(char)
}

let tekst = "Witaj Świecie!"
let wynik = String.map(map_lower, tekst)

assert wynik == "witaj światem!"
```

Podczas korzystania z biblioteki standardowej języka Gleam, warto zwracać uwagę na dostępne funkcje, które mogą ułatwić nam pracę i zwiększyć czytelność naszego kodu.

## Zobacz także

- Dokumentacja języka Gleam: https://gleam.run/
- Zaimplementowane funkcje dla stringów w bibliotece standardowej: https://github.com/lpil/gleam_stdlib/tree/master/String