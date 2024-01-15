---
title:                "Konwertowanie ciągu na wielkie litery"
html_title:           "Go: Konwertowanie ciągu na wielkie litery"
simple_title:         "Konwertowanie ciągu na wielkie litery"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w naszym programie chcielibyśmy zmienić pierwszą literę każdego słowa w zdaniu na wielką. Możemy to osiągnąć przez użycie funkcji do zmiany wielkości liter, ale jeśli chcemy tylko zmienić pierwszą literę, ta metoda może być zaawansowana. Wtedy przydatne może się okazać użycie funkcji do kapitalizacji ciągów znaków w języku Go.

## Jak to zrobić

Możemy wykorzystać funkcję "Title" do kapitalizacji ciągów znaków w języku Go. W poniższym przykładzie wykorzystamy funkcję do kapitalizowania słowa "hello":

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    fmt.Println(strings.Title("hello"))
}

// Output: Hello
```

Możemy również wykorzystać funkcję "ToUpper" do zmiany pierwszej litery na wielką. Poniższy kod przedstawi to w kontekście całego zdania:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hello world"
    result := strings.ToUpper(str[0:1]) + str[1:]
    fmt.Println(result)
}

// Output: Hello world
```

## Głębszy wgląd

W języku Go istnieje wiele funkcji do manipulacji ciągami znaków. Funkcje "Title" i "ToUpper" to tylko kilka przykładów, ale istnieje wiele innych sposobów na kapitalizowanie ciągów znaków w języku Go. Możemy również zastosować inne operacje, takie jak "Split" i "Join", aby dostosować nasz ciąg znaków zgodnie z naszymi potrzebami.

## Zobacz również

- [Funkcja "Title" w języku Go](https://golang.org/pkg/strings/#Title)
- [Funkcja "ToUpper" w języku Go](https://golang.org/pkg/strings/#ToUpper)
- [Manipulacja ciągami znaków w języku Go](https://golang.org/pkg/strings/)