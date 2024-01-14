---
title:                "Go: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie jest nieodłączną częścią codziennej pracy wielu osób. Często spotykamy się z koniecznością zmieniania dużych ilości tekstu w naszym kodzie. Dlatego warto poznać sposoby na efektywne wyszukiwanie i wymienianie tekstu w języku Go.

## Jak to zrobić

Pierwszym krokiem jest skupienie się na funkcji `strings.Replace()`, która jest wbudowana w język Go. Pozwala ona na wyszukiwanie i zamienianie określonych fragmentów tekstu w danej napisanej linii kodu. Poniżej znajduje się przykład kodu, który demonstrować będzie tę funkcję:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  text := "Witamy w świecie Go!"
  newText := strings.Replace(text, "Go", "programowania", 1)
  fmt.Println(newText)
}

// output: Witamy w świecie programowania!
```

Jak widać, funkcja `strings.Replace()` przyjmuje trzy argumenty - oryginalny tekst, szukaną frazę oraz tekst, na który ma zostać zamieniona szukana fraza. Dodatkowo, dodaliśmy trzeci argument w postaci liczby `1`, co oznacza, że zamiana wykona się tylko raz dla pierwszego napotkanego wystąpienia frazy "Go". W ten sposób możemy precyzyjnie kontrolować zamienianie tekstu w naszym kodzie.

## Głębsze zagadnienia

W języku Go istnieje również funkcja `strings.ReplaceAll()`, która pozwala na globalne zastępowanie określonej frazy we wszystkich jej wystąpieniach w tekście. Poniżej przedstawiono jej prosty przykład:

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  text := "To jest przykładowy tekst, tekst, tekst!"
  newText := strings.ReplaceAll(text, "tekst", "teksty")
  fmt.Println(newText)
}

// output: To jest przykładowy teksty, teksty, teksty!
```

Dodatkowo, w języku Go istnieje możliwość wykorzystania wyrażeń regularnych do jeszcze bardziej precyzyjnego wyszukiwania i zamiany tekstu. W tym przypadku, należy wykorzystać pakiet `regexp` oraz funkcję `ReplaceAllString()`.

## Zobacz również

- [Dokumentacja funkcji strings.Replace() w języku Go](https://golang.org/pkg/strings/#Replace)
- [Dokumentacja funkcji strings.ReplaceAll() w języku Go](https://golang.org/pkg/strings/#ReplaceAll)
- [Dokumentacja pakietu regexp w języku Go](https://golang.org/pkg/regexp/)