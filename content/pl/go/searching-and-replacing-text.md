---
title:                "Wyszukiwanie i zamiana tekstu"
date:                  2024-01-20T17:57:47.839614-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Szukanie i zamiana tekstu to jak patrol z psem w labiryncie słów – znajdujemy coś konkretnego i wymieniamy na nowe. Programiści to robią, żeby poprawić błędy, aktualizować dane czy po prostu uporządkować kod.

## Jak to zrobić:
Wyszukiwanie i zamiana w Go jest prosta jak przysłowiowa bułka z masłem. Do dzieła:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	tekst := "To jest przykład tekstu, w którym będzie zamiana."
	szukaj := "zamiana"
	zamienNa := "podmianka"
	
	// Szukanie i zamiana tekstu
	nowyTekst := strings.Replace(tekst, szukaj, zamienNa, -1)

	fmt.Println(nowyTekst) // To jest przykład tekstu, w którym będzie podmianka.
}
```

## Deep Dive
W Go szukanie i zamiana tekstu opiera się na pakiecie `strings`. Przed `strings.Replace` były inne sposoby, jak regex, ale to była nierzadko nadwyżka możliwości. Istnieją też alternatywy jak `bytes.Replace` dla dużych danych. Właściwe narzędzie zależy od wielkości tekstu i wymaganej kontroli nad procesem.

Mechanizm używany w `strings.Replace` to iteracja po tekście, co może być nieefektywne przy wielkich tomach danych. Tutaj nadejdzie z pomocą `strings.Replacer`, który pozwala zdefiniować wiele wyrażeń do zamiany i używa efektywniejszego algorytmu.

## Zobacz też:

- [Pakiet strings](https://pkg.go.dev/strings)
- [Regexp w Go](https://pkg.go.dev/regexp)
- [Replacer z pakietu strings](https://pkg.go.dev/strings#Replacer)