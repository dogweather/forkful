---
title:                "Interpolacja ciągu znaków."
html_title:           "Go: Interpolacja ciągu znaków."
simple_title:         "Interpolacja ciągu znaków."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Interpolowanie łańcucha znaków to prosta i przydatna funkcja w programowaniu, która pozwala na wstawianie wartości zmiennych do łańcucha znaków. Programiści używają go, aby dynamicznie zmieniać tekstowy wyjście swoich programów.

## Jak to zrobić?

Interpolacja w Go jest obsługiwana przez specjalne znaki ```%v``` i ```%T```. Możesz użyć ich do wstawiania wartości zmiennych do łańcucha znaków w następujący sposób:

```Go
favoriteColor := "blue"
fmt.Printf("Mój ulubiony kolor to %v", favoriteColor)
```

Wyświetli to: `Mój ulubiony kolor to blue`.

## Dogłębnie

Interpolacja łańcucha znaków jest popularną techniką, która została zapoczątkowana w języku Pascal i jest szeroko stosowana w wielu innych językach programowania. Alternatywą dla interpolacji jest konkatenacja, czyli łączenie łańcuchów znaków za pomocą operatora `+`.

W Go interpolacja jest obsługiwana przez pakiet `fmt` i funkcję `Printf`. Jest to zaimplementowane w języku C, więc dezinformacja na temat typów może spowodować błędy.

## Zobacz też

- [Official Go documentation on string formatting](https://golang.org/pkg/fmt/#hdr-Printing)