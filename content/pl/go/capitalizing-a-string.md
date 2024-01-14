---
title:                "Go: Zmiana formatowania ciągu znaków"
simple_title:         "Zmiana formatowania ciągu znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Witajcie polscy programiści! Jeśli regularnie pracujecie z językiem Go, zapewne spotkaliście się z potrzebą zmiany wielkości liter w napisach. W tym wpisie opowiem Wam dlaczego to jest ważne i jak można to zrobić w prosty sposób.

## Jak to zrobić

W języku Go zmiana wielkości liter jest możliwa dzięki użyciu funkcji `strings.Title()` i `strings.ToUpper()`. Dzięki nim możemy zamienić pierwszą literę lub wszystkie litery w wybranym napisie na wielkie. Oto kilka przykładów:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    input := "witajcie polscy programiści"
    fmt.Println(strings.Title(input))

    input = "Go to jest super język"
    fmt.Println(strings.ToUpper(input))
}
```

Output:

```
Witajcie Polscy Programiści
GO TO JEST SUPER JĘZYK
```

Jak widać, zmiana wielkości liter jest bardzo prosta i nie wymaga wielu linii kodu.

## Deep Dive

W języku Go możemy zmieniać wielkość liter nie tylko w całych napisach, ale także w pojedynczych literach. Możemy to zrobić dzięki użyciu typu `rune`, który reprezentuje pojedynczą literkę wewnętrznie jako liczbę. Dzięki temu możemy wykonywać różne operacje na literach, np. zmienić tylko pierwszą literę na wielką. Przykład:

```Go
package main

import "fmt"

func main() {
    letter := 'a'
    fmt.Printf("Pierwsza litera: %c\n", letter)

    // zamiana na wielkie litery
    letter -= 32
    fmt.Printf("Pierwsza litera: %c", letter)
}
```

Output:

```
Pierwsza litera: a
Pierwsza litera: A
```

Warto także pamiętać, że w języku Go litery są reprezentowane przy użyciu kodu Unicode, dzięki czemu możemy pracować z różnymi alfabetami.

## Zobacz także

- Dokumentacja języka Go: https://golang.org/doc/
- Przewodnik po zmianie wielkości liter w Go: https://gobyexample.com/string-functions
- Opis typu `rune`: https://golang.org/ref/spec#Rune_literals