---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Reguły wyrażeń regularnych (regex) pozwalają szukać wzorców w tekście. Programiści używają regex do weryfikacji, wyszukiwania i manipulacji danymi tekstowymi.

## Jak to zrobić:
```Go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	emailRegex := regexp.MustCompile(`^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,4}$`)
	email := "przyklad@domena.pl"
	match := emailRegex.MatchString(email)
	fmt.Printf("Czy '%s' to poprawny email? %v\n", email, match)
	
	searchPattern := regexp.MustCompile(`\bGo\b`)
	text := "Uczę się Go i używam Go w codziennej pracy."
	matches := searchPattern.FindAllString(text, -1)
	fmt.Printf("Znalezione dopasowania: %v\n", matches)
}
```
Wyjście:
```
Czy 'przyklad@domena.pl' to poprawny email? true
Znalezione dopasowania: [Go Go]
```

## Deep Dive
Wyrażenia regularne powstały w latach 50. Alternatywami dla regex są parser textu lub wyszukiwanie za pomocą metod znakowych. W Go, regex implementowany jest przez pakiet `regexp`, który korzysta z silnika RE2, zapewniającego bezpieczeństwo przed słynnym problemem "catastrophic backtracking".

## Zobacz również:
- Dokumentacja Go `regexp` package: https://golang.org/pkg/regexp/
- Tutorial o wyrażeniach regularnych: https://www.regular-expressions.info/tutorial.html
- Testowanie wyrażeń regularnych online: https://regex101.com/