---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wzorce wyrażeń regularnych są jak filtr do przeszukiwania tekstu. Pozwalają programistom na szybkie znajdowanie, weryfikowanie i manipulowanie danymi w stringach. Dlaczego? Bo czas to pieniądz, a wyrażenia regularne oszczędzają oba.

## How to: (Jak to zrobić:)
```gleam
import gleam/regex

fn main() {
  let pattern = regex.from_string("[a-zA-Z]+") // Wzorzec dla słów
  // Szukajmy wszystkich pasujących
  let result = regex.find_all(pattern, "Hello, Gleam!")
  // Wynik
  case result {
    Ok(matches) -> matches 
    Error(_) -> []
  }
}
```
Sample output (Przykładowy wynik):
```gleam
["Hello", "Gleam"]
```

## Deep Dive (Dogłębna analiza)
Wyrażenia regularne mają korzenie w teorii automatów i języków formalnych. Alternatywy? Parser tekstowy dla bardziej skomplikowanych zadań. W Gleamie wykorzystywane jest moduł `gleam/regex`, który opiera się na silniku wyrażeń regularnych z Erlanga.

## See Also (Zobacz też)
- Dokumentacja `gleam/regex`: [https://hexdocs.pm/gleam_stdlib/gleam/regex](https://hexdocs.pm/gleam_stdlib/gleam/regex)
- Interaktywny edytor wyrażeń regularnych: [https://regex101.com/](https://regex101.com/)
- Krótki kurs wyrażeń regularnych: [https://regexone.com/](https://regexone.com/)