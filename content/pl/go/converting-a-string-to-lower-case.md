---
title:                "Go: Konwersja ciągu znaków na małe litery"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego
 Konwersja ciągu znaków na małe litery może być niezbędna w wielu sytuacjach w programowaniu. Na przykład, gdy chcemy porównać dwa ciągi znaków bez względu na wielkość liter, używając funkcji konwersji na małe litery możemy uprościć ten proces.

## Jak to zrobić
```Go
package main

import "fmt"
import "strings"

func main() {
    str1 := "PROGRAMOWANIE"
    fmt.Println("Pierwotny ciąg: ", str1)
    fmt.Println("Ciąg po konwersji na małe litery: ", strings.ToLower(str1))

    str2 := "Go"
    fmt.Println("Pierwotny ciąg: ", str2)
    fmt.Println("Ciąg po konwersji na małe litery: ", strings.ToLower(str2))
}
```
Output:
```
Pierwotny ciąg: PROGRAMOWANIE
Ciąg po konwersji na małe litery: programowanie
Pierwotny ciąg: Go
Ciąg po konwersji na małe litery: go
```

## Głębszy wykład
Konwersja ciągu znaków na małe litery w języku Go może zostać wykonana za pomocą funkcji `strings.ToLower ()`. Jest to funkcja, która zwraca kopię ciągu znaków z wszystkimi literami przekształconymi na małe. Ważne jest, aby pamiętać, że funkcja ta nie zmienia oryginalnego ciągu znaków, ale zwraca jego zmienioną kopię. Oznacza to, że oryginalny ciąg pozostaje bez zmian, a funkcja zwraca nowy ciąg. 

## Zobacz również
- Dokumentacja języka Go na temat konwersji ciągów znaków: https://golang.org/pkg/strings/#ToLower
- Porównywanie ciągów znaków bez uwzględniania wielkości liter w języku Go: https://www.golangprograms.com/go-language/builtin-package/strings.html#adf
- Dyskusja na temat różnych metod konwertowania ciągów znaków na małe litery w języku Go: https://stackoverflow.com/questions/45668712/golang-why-is-strings-tolower-preferred-over-strings-map-unicode-tolower