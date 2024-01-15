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

## Dlaczego
Zastanawiałeś się kiedyś jak programy są w stanie wykryć długość tekstu lub słowa? W tym artykule dowiecie się o prostym sposobie wykorzystania Go do znajdowania długości stringa.

## Jak
Jednym z najprostszych sposobów znajdowania długości stringa w Go jest użycie funkcji `len()`. Spójrz na poniższy przykład:

```Go
package main

import "fmt"

func main() {
    str := "Witaj świecie!"
    length := len(str)
    fmt.Println(length)
}
```

Wyjście: `14` 

Funkcja `len()` przyjmuje string jako argument i zwraca jego długość. Jest to bardzo proste i wydajne rozwiązanie dla większości przypadków. 

Jednak jeśli chcesz upewnić się, że długość liczona jest w znakach, a nie w bajtach, możesz użyć funkcji `utf8.RuneCountInString()`:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Привет мир!"
    length := utf8.RuneCountInString(str)
    fmt.Println(length)
}
```

Wyjście: `11`

Funkcja ta uwzględnia różnice w kodowaniu znaków i zwraca właściwą długość bez względu na użyty język. 

## Deep Dive
W Go wewnętrznie string jest reprezentowany jako tablica bajtów, dlatego funkcja `len()` zwraca liczbę bajtów, a nie znaków. Dlatego, jeśli będziesz pracował z kodowaniem znaków, należy użyć funkcji `utf8.RuneCountInString()`.

Należy również pamiętać, że stringi są typu immutable w Go, co oznacza, że nie można zmienić istniejącej wartości stringa. W przypadku zmiany długości, zostanie utworzony nowy string z nowymi wartościami, a stary zostanie usunięty. W związku z tym, jeśli masz do czynienia z dużymi ilościami stringów, lepiej użyć typu `[]byte` zamiast typu `string`, ponieważ ten pierwszy jest mutable i możliwe jest modyfikowanie jego długości bez konieczności tworzenia nowego obiektu w pamięci.

## Zobacz też
- [Dokumentacja Go: func Len](https://golang.org/pkg/builtin/#len)
- [Dokumentacja Go: func RuneCountInString](https://golang.org/pkg/unicode/utf8/#RuneCountInString)
- [Blog Go: Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)