---
title:                "Zamiana liter w ciągu na wielkie"
html_title:           "Go: Zamiana liter w ciągu na wielkie"
simple_title:         "Zamiana liter w ciągu na wielkie"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zmiana pierwszej litery ciągu na wielką to tzw. kapitalizacja. Programiści robią to, aby zachować poprawną gramatykę w interfejsach użytkownika lub do porównywania ciągów bez uwzględnienia wielkości liter.

## Jak to zrobić:
W Go, możemy użyć wbudowanej funkcji `strings.Title()` do kapitalizacji ciągu. Oto jak:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "witaj, świecie!"
	result := strings.Title(str)
	fmt.Println(result)
}
```

Wyjście:
```Go
Witaj, Świecie!
```

## Głębsze zanurzenie
Kapitalizacja ciągów ma swoje korzenie w dawnych językach programowania i jest powszechnie stosowana w różnych językach, nie tylko w Go. 

Alternatywą do `strings.Title()` w Go jest napisanie własnej funkcji. Na przykład, możemy zastosować `unicode.ToTitle()`, który konwertuje jednoznakowe runy.

Detale implementacji `strings.Title()` polegają na kolejnym iterowaniu po ciągu i zamianie każdej litery, która znajduje się po nie-literze, na literę wielką. 

## Zobacz też
[Go Docs: Pakiet „strings”](https://pkg.go.dev/strings)  
[Dokumentacja Go: unicode.ToTitle](https://golang.org/pkg/unicode/#ToTitle)