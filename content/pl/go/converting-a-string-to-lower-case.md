---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-01-20T17:38:18.896433-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Konwersja napisu do małych liter to proces zmiany wszystkich dużych liter w ciągu znaków na ich małe odpowiedniki. Robimy to dla jednolitości, uproszczenia wyszukiwania czy porównywania tekstów.

## Jak to zrobić:
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Witaj Świecie!"
	lowercased := strings.ToLower(original)
	fmt.Println(lowercased)
}
```
Wyjście:
```
witaj świecie!
```

## Dogłębna analiza:
Historia funkcji toLower sięga pierwszych funkcji przetwarzania tekstu, gdzie jednoznaczność i standardy były kluczowe dla uproszczenia operacji na napisach. Alternatywą dla `strings.ToLower` może być ręczne iterowanie po znakach i konwersja przy użyciu własnej funkcji, ale jest to bardziej skomplikowane i mniej wydajne. W Go, implementacja `ToLower` uwzględnia lokalizację, dzięki czemu działanie jest poprawne również dla znaków specyficznych dla języków inne niż angielski, np. polskich.

## Zobacz także:
- Dokumentacja Go na temat pakietu strings: [Package strings](https://pkg.go.dev/strings)
- Unicode Standard: [Unicode](http://www.unicode.org/standard/standard.html)
