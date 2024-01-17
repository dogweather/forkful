---
title:                "Pisanie do standardowego błędu"
html_title:           "Go: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie do standardowego wyjścia błędów to sposób na wyświetlanie błędnych informacji lub ostrzeżeń podczas pracy programu. Programiści często wykorzystują to do debugowania i poprawiania swoich kodów.

## Jak to zrobić:

Aby wypisać informacje do standardowego wyjścia błędów w języku Go, wystarczy użyć funkcji `fmt.Fprintln(os.Stderr, message)`, gdzie `message` jest tekstem lub zmienną zawierającą informację do wyświetlenia. Przykład:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    message := "Nieznany błąd: nie udało się otworzyć pliku."
    fmt.Fprintln(os.Stderr, message)
}
```

Output:
```
Nieznany błąd: nie udało się otworzyć pliku.
```

## W głębszej analizie:

Pisanie do standardowego wyjścia błędów jest często wykorzystywane do debugowania i raportowania błędów podczas działania programu. Alternatywnym sposobem może być zapisywanie błędów do pliku lub zwracanie ich jako wyjątków. W języku Go istnieje także funkcja `panic()`, która przerywa działanie programu i wyświetla błędy. Jednak pisanie do standardowego wyjścia błędów jest prostsze i bardziej czytelne.

## Zobacz także:

- [Dokumentacja języka Go](https://golang.org/doc/)
- [Wprowadzenie do standardowego wyjścia błędów w języku Go](https://tutorialedge.net/golang/io-writer-interface-tutorial/)