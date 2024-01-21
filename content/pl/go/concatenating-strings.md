---
title:                "Łączenie łańcuchów znaków"
date:                  2024-01-20T17:34:58.923605-07:00
model:                 gpt-4-1106-preview
simple_title:         "Łączenie łańcuchów znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Łączenie łańcuchów znaków to po prostu składanie razem dwóch lub więcej ciągów tekstów. Programiści robią to, by dynamicznie tworzyć treści, jak wiadomości, dynamiczne URL czy kod.

## How to: (Jak to zrobić:)
```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Przykład 1: Operator plus (+)
	hello := "Cześć"
	world := "Świecie"
	helloWorld := hello + ", " + world + "!"
	fmt.Println(helloWorld) // Wynik: Cześć, Świecie!

	// Przykład 2: Funkcja Sprintf z pakietu fmt
	greeting := fmt.Sprintf("%s, %s!", hello, world)
	fmt.Println(greeting) // Wynik: Cześć, Świecie!

	// Przykład 3: Funkcja Join z pakietu strings
	parts := []string{"Cześć", "Świecie"}
	joinedString := strings.Join(parts, ", ")
	fmt.Println(joinedString) // Wynik: Cześć, Świecie
}
```

## Deep Dive (W Głąb Tematu)
Historia łączenia łańcuchów znaków sięga początków programowania, kiedy tworzenie większych bloków tekstu wymagało spajania mniejszych części. W Go, operator `+` jest najprostszą metodą, ale nadaje się głównie do prostych operacji. `fmt.Sprintf` jest bardziej elastyczny, pozwala na formatowanie tekstu z wartościami zmiennych. Natomiast `strings.Join` jest optymalny dla łączenia długich list elementów.

Implementacyjnie, ważne jest, by pamiętać, że w Go każde łączenie stringów tworzy nowy łańcuch, co może być kosztowne dla pamięci i wydajności. Dlatego przy dużych i częstych operacjach warto użyć `strings.Builder`, który jest efektywniejszy.

## See Also (Zobacz też)
- Dokumentacja Go poświęcona pakietowi `strings`: https://golang.org/pkg/strings/
- Porady dotyczące efektywności w łączeniu łańcuchów znaków w Go: https://blog.golang.org/strings
- Opis `strings.Builder`: https://golang.org/pkg/strings/#Builder