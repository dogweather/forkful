---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego i po co?

Usunięcie znaków pasujących do wzorca ma na celu manipulację tekstami w celu uzyskania pozbycia się niepotrzebnych znaków w ciągu danych. Programiści robią to, gdy pragną oczyszczenia swojego tekstu z niechcianych lub zbędnych znaków.

## Jak to zrobić:

Go zapewnia wbudowaną funkcję `strings.ReplaceAll(s, old, new)`, która jest używana do zastępowania wszystkich wystąpień podciągu `old` w ciągu `s` na ciąg `new`. 

Sprawdźmy to na przykładzie:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	str := "Witaj, Witaj, świecie!"
	
	// Usunięcie wszystkich liter 'W' ze ciągu
	newStr := strings.ReplaceAll(str, "W", "")
	
	fmt.Println(newStr) // "itaj, itaj, świecie!"
}
```

## Pogłębione informacje:

Usunięcie znaków pasujących do wzorca jest częstą praktyką programistyczną od czasów, gdy zaczęto manipulować danymi tekstowymi. 

Co do alternatyw, możemy używać funkcji `regexp.ReplaceAllString(src, repl)`, która pozwala na bardziej skomplikowane operacje usuwania znaków.

Oto szczegóły implementacji dla funkcji `strings.ReplaceAll(s, old, new)`. Ta funkcja najpierw wyszukuje indeksy wszystkich wystąpień `old` w ciągu `s` za pomocą algorytmu wyszukiwania tekstu Boyer-Moore'a. Następnie, tworzy nowy ciąg, kopiując fragmenty ciągu `s` między tymi indeksami, a następnie wstawia `new` zamiast `old`.

## Zobacz też:

1. Dokumentacja Go dla pakietu "strings": https://golang.org/pkg/strings/
2. Dokumentacja Go dla pakietu "regexp": https://golang.org/pkg/regexp/
3. Algorytm Boyer-Moore’a: https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm