---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyodrębnianie substringów polega na wyciąganiu fragmentów z większego ciągu znaków. Programiści robią to dla manipulacji danymi i przekształcania informacji zawartych w ciągach znaków w użyteczne dane.

## Jak to zrobić:

```Go
package main
import "fmt"

func main() {
	str := "Witaj, Świecie Go"
	subStr := str[7:13]
	fmt.Println(subStr) 
}
```
Powinno zwrócić: "Świecie"

Jasne, co? Dodatkowo możemy używać funkcji `strings` w Go do wyodrębniania substringów.

```Go
package main
import (
	"fmt"
	"strings"
)

func main() {
	str := "Witaj, Świecie Go"
	subStr := strings.Split(str, ",")[1]
	fmt.Println(subStr)
}
```
Zwraca: " Świecie Go"

## Deep Dive
Wyodrębnianie substringów jest fundamentalnym aspektem manipulacji tekstem, używanym od początków programowania. Go korzysta z indeksowania łańcuchów bajtów za pomocą indeksów i wykorzystuje wydajne operacje przycinania. Alternatywą jest użycie paczki `strings`, która obejmuje wiele funkcji, w tym `Split`, `Contains` itp., co zapewnia większą kontrolę nad procesem.

Podczas wyodrębniania substringów warto pamiętać, że Go interpretuje stringi jako ciąg bajtów, dlatego podczas pracy z multibajtowymi znakami (takimi jak UTF-8) mogą wystąpić problemy.

## Zobacz też:
- Dokumentacja Go na temat pakietu Strings: https://golang.org/pkg/strings/
- Artykuł na Medium o manipulacji stringami w języku Go: https://medium.com/go-walkthrough/go-walkthrough-strings-package-4985b3bb22bb
- Kurs Codecademy dotyczący przetwarzania tekstu w Go: https://www.codecademy.com/learn/learn-go/modules/learn-go-string-and-console-output