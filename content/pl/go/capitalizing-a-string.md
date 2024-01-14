---
title:    "Go: Zmiana wielkości ciągu znaków"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy zmieniać format tekstu, na przykład zamieniając pierwszą literę każdego wyrazu na wielką. W tym artykule dowiesz się, jak w języku Go można skutecznie zmieniać wielkość liter w łańcuchach znaków.

## Jak to zrobić

Możemy użyć funkcji `strings.Title()` do zmiany pierwszej litery każdego wyrazu na wielką. Należy pamiętać, że ta funkcja nie zmienia pozostałych liter na małe. Przejdźmy przez przykład, aby to zobaczyć:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "programowanie w go jest świetne"
	fmt.Println(strings.Title(s))
}
```
**Output:** Programowanie W Go Jest Świetne

Jeśli chcemy zmienić wszystkie litery na duże, możemy skorzystać z funkcji `strings.ToUpper()`:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "programowanie w go jest świetne"
	fmt.Println(strings.ToUpper(s))
}
```
**Output:** PROGRAMOWANIE W GO JEST ŚWIETNE

Kolejną opcją jest używanie pętli `for` i funkcji `strings.ToUpper()` do zmiany wielkości każdej litery:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	s := "programowanie w go jest świetne"
	result := ""

	for _, letter := range s {
		result += strings.ToUpper(string(letter))
	}

	fmt.Println(result)
}
```
**Output:** PROGRAMOWANIE W GO JEST ŚWIETNE

## Głębsza analiza

W języku Go funkcje z pakietu `strings` są oparte na standardzie Unicode. Dzięki temu pisanie kodu do manipulacji łańcuchami jest prostsze i bardziej efektywne. Warto również pamiętać, że funkcje te są niezmiennikowe, co oznacza, że zawsze zwracają nowe łańcuchy, a nie zmieniają istniejących. Dzięki temu unikamy niepożądanych efektów ubocznych.

## Zobacz również

- Dokumentacja funkcji `strings.Title()`: https://golang.org/pkg/strings/#Title
- Dokumentacja funkcji `strings.ToUpper()`: https://golang.org/pkg/strings/#ToUpper
- Dokumentacja pętli `for` w języku Go: https://golang.org/ref/spec#For_statements