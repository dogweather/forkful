---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Kapitalizowanie stringa to proces zamiany pierwszych liter wyrazów na duże. Robimy to dla poprawienia czytelności, formalności lub spraw by tekst był bardziej wpadający w oko.

## How to: (Jak Zrobić:)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	text := "żółw szybko biega po plaży"
	capitalizedText := strings.Title(text)
	fmt.Println(capitalizedText) // Żółw Szybko Biega Po Plaży
}
```

## Deep Dive (Dogłębna Analiza)
W Go, strings.Title() jest często używany do kapitalizacji każdego wyrazu w stringu, ale ma swoje ograniczenia - postrzega każdą literę po spacji jako początek nowego słowa. Dla dokładniejszej kontroli, stosuje się własne funkcje. Dawniej, gdy jeszcze nie było takich funkcji jak `Title`, kapitalizacja wymagała więcej ręcznej pracy.

Rozważ używanie `strings.ToTitle` gdy potrzebujesz WIELKICH LITER. Pamiętaj też, że kapitalizacja może zmieniać znaczenie wyrazów w niektórych językach, więc używaj z uwagą.

Alternatywą jest `text.Title` z pakietu "golang.org/x/text", który oferuje pełniejsze wsparcie dla języków i unikodowych reguł kapitalizacji.

Za kulisami, funkcje jak `strings.Title` działają poprzez iterację po stringu i aplikacje transformacji do kodów unicode znaków, bazując na prostych regułach.

## See Also (Zobacz Również)
- Dokumentacja Go strings pakietu: https://pkg.go.dev/strings
- Pakiet 'text' z extended support: https://pkg.go.dev/golang.org/x/text
- Unicode Standard on Case: http://unicode.org/reports/tr21/
