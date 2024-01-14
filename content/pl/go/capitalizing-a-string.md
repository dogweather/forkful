---
title:    "Go: Zmiana liter w ciągu znaków"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś chciałby zmienić wielkość liter w ciągu znaków? Czasami może być to konieczne, np. w celu stworzenia spójnego formatowania tekstu lub w przypadku sprawdzania danych wprowadzonych przez użytkownika.

## Jak to zrobić

Istnieje prosty sposób na zmianę wielkości liter w ciągu znaków przy użyciu języka Go. Możemy to zrobić za pomocą funkcji `strings.ToUpper()` lub `strings.ToLower()`, w zależności od tego, czy chcemy zmienić wszystkie litery na duże czy małe. Przykładowy kod wyglądałby następująco:
```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    fmt.Println(strings.ToUpper("Cześć Świecie!"))
    fmt.Println(strings.ToLower("Cześć Świecie!"))
}
```
Oto jak wyglądałby wynik tego kodu:
```
CZEŚĆ ŚWIECIE!
cześć świecie!
``` 
W ten sposób możemy szybko i łatwo zmienić wielkość liter w ciągu znaków w naszych programach.

## Głębszy zanurzenie

Aby lepiej zrozumieć, jak funkcje `ToUpper()` i `ToLower()` działają w języku Go, warto przyjrzeć się szczegółom. Funkcja `ToUpper()` wykorzystuje standardową bibliotekę `strings`, która zawiera wiele przydatnych metod do manipulacji tekstem. W przypadku zmiany wielkości liter, funkcja ta przeszukuje cały ciąg znaków i zamienia każdą małą literę na wielką. Funkcja `ToLower()` działa w podobny sposób, ale zamienia wszystkie litery na małe.

## Zobacz też

Biblioteka `strings` w języku Go oferuje wiele innych przydatnych funkcji do pracy z tekstem. Możesz przeczytać więcej o niej na stronie dokumentacji: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)

Jeśli interesują Cię inne sposoby manipulacji tekstem w języku Go, możesz również zapoznać się z tym artykułem: [https://golangnews.com/stories/2019-go-how-to-pad-print-strings](https://golangnews.com/stories/2019-go-how-to-pad-print-strings)