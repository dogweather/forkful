---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Go: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Znalezienie długości łańcucha znaków (string) to operacja, która polega na policzeniu ilości znaków w danym tekście. Wykonują ją programiści w celu mierzenia i porównywania długości różnych łańcuchów znaków w swoich programach.

## Jak to zrobić:
```Go
package main

import "fmt"

func main() {
	s := "Witaj, świecie!" // deklaracja łańcucha znaków
	fmt.Println(len(s)) // wypisanie długości łańcucha
}

// Output: 15
```

## Głębszy wgląd:
Operacja znajdowania długości łańcucha znaków jest niezbędna w wielu programach. Wcześniej nie było to zadanie prostego, ponieważ programiści musieli sami zaimplementować funkcje przechodzenia po łańcuchu i liczenia poszczególnych znaków. Jednak dzięki językom programowania takim jak Go, ta operacja jest teraz znacznie prostsza.

Alternatywnym rozwiązaniem jest użycie funkcji wbudowanej w język Go, czyli ```len()```, która działa podobnie do funkcji z przykładu powyżej. Jednak istnieje również wiele różnych sposobów, w jaki programiści mogą zaimplementować tę operację, dlatego warto eksperymentować i szukać najlepszego rozwiązania dla swojego projektu.

## Zobacz także:
- Dokumentacja języka Go dotycząca funkcji ```len()```: https://golang.org/pkg/builtin/#len
- Wideo tutorial "Go Tutorial #9 - String Length": https://www.youtube.com/watch?v=tBAKu0mZI0Y
- Strona internetowa String Functions w języku Go: https://golang.org/pkg/strings/