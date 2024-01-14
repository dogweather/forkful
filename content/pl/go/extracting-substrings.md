---
title:                "Go: Wycinanie podciągów"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ekstrakcja podłańcuchów może być bardzo przydatna w programowaniu w języku Go. W tej krótkiej instrukcji dowiesz się, dlaczego jest to przydatna umiejętność i jak wykorzystać ją w praktyce.

## Jak

Aby ekstrakcja podłańcuchów była możliwa w języku Go, musimy znać kilka podstawowych funkcji. Jedną z najważniejszych jest funkcja "string.Index", która zwraca indeks pierwszego wystąpienia podłańcucha w całym ciągu znaków. W połączeniu z funkcją "string.Split", która dzieli dany ciąg znaków na poszczególne podłańcuchy, możemy wygodnie wyodrębnić interesujące nas fragmenty tekstu.

Przykład:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Przykładowy tekst do ekstrakcji podłańcuchów"
    substr := "tekst"

    index := strings.Index(str, substr)
    fmt.Println("Indeks pierwszego wystąpienia podłańcucha:", index)

    subStrArr := strings.Split(str, " ")
    fmt.Println("Podłańcuchy uzyskane przy dzieleniu:", subStrArr)
}
```

Wyjście:

Indeks pierwszego wystąpienia podłańcucha: 12
Podłańcuchy uzyskane przy dzieleniu: [Przykładowy, tekst, do, ekstrakcji, podłańcuchów]

## Deep Dive

Aby jeszcze lepiej wykorzystać ekstrakcję podłańcuchów, warto poznać inne funkcje związane z operacjami na łańcuchach znaków. Na przykład funkcja "strings.Replace" pozwala na zamianę wybranej części tekstu na inny podłańcuch, co może być przydatne przy edycji danych tekstowych. Z kolei funkcja "strings.ToUpper" umożliwia konwersję tekstu na wielkie litery, a "strings.Trim" usuwa wybrane znaki z początku i końca ciągu.

## Zobacz również

- Dokumentacja języka Go: https://golang.org/
- Przewodnik po podstawach języka Go: https://tour.golang.org/
- Przykładowe programy w języku Go: https://github.com/golang/go/wiki/Projects