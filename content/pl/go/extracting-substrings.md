---
title:                "Wycinanie podłańcuchów"
date:                  2024-01-20T17:46:07.100693-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
Wyciąganie podciągów to technika wydobywania określonych fragmentów tekstu z większego ciągu znaków. Programiści używają jej do analizy danych, walidacji, czy manipulacji tekstami bez konieczności przetwarzania całego ciągu.

## Jak to zrobić:
Extrahowanie podciągów w Go jest proste. Użyjmy `slice` operacji na stringach, by ogarnąć podstawy.

```go
package main

import (
	"fmt"
)

func main() {
	text := "Witaj, świecie!"
	
	// Pobierz podciąg od 7. do 13. znaku (indeksy od 0)
	substr := text[7:14]
	fmt.Println(substr) // Output: świecie

	// Jeśli chcesz zacząć od początku, pomiń pierwszą liczbę
	fmt.Println(text[:5]) // Output: Witaj

	// Pominięcie drugiej liczby oznacza ciąg do końca
	fmt.Println(text[7:]) // Output: świecie!
}
```
Pamiętaj o pojęciu "rune" w Go. Jeśli tekst zawiera znaki specjalne lub Unicode, lepiej użyj `for range` albo funkcji z pakietu `unicode/utf8` by zachować ciągi znaków poprawnie.

## Głębsze spojrzenie:
Extrahowanie podciągów to coś, co ludzie robili od czasów przetwarzania danych na kartach dziurkowanych. W Go, traktowanie ciągów znaków jako odpowiedników tablic bajtów sprawia, że operacje są szybkie. Konieczne jest jednak uwzględnianie Unicode i kodowania UTF-8, co dodaje warstwę złożoności.

Alternatywą do "ręcznego" wyciągania podciągów jest użycie funkcji z pakietów takich jak `strings` czy `regexp`, które zawierają przydatne narzędzia do szukania i manipulacji tekstami.

Pod względem implementacji, ważne jest by pamiętać, że Go używa modelu opartego na bajtach, nie na znakach, co różni się od niektórych innych języków, gdzie string jest kolekcją znaków (np. Java).

## Zobacz także:
- Dokumentacja pakietu `strings`: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
- Dokumentacja pakietu `unicode/utf8`: [https://golang.org/pkg/unicode/utf8/](https://golang.org/pkg/unicode/utf8/)
- Artukuł o pracy z UTF-8 w Go: [https://blog.golang.org/strings](https://blog.golang.org/strings)
- Dokumentacja dla `regexp` dla bardziej zaawansowanej manipulacji ciągami: [https://golang.org/pkg/regexp/](https://golang.org/pkg/regexp/)
