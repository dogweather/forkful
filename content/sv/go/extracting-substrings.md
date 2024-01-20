---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Extrahera substrängar innebär att hämta ut en del av en ursprunglig sträng. Programmerare gör detta för att använda specifika portioner av data, likt att segmentera en större mängd information i hanterbara bitar.

## Hur till:

```Go
package main
import "fmt"

func main() {
    str := "Jag älskar att programmera i Go."
    substr := str[0:9]
    fmt.Println(substr)
}
```
`Output: Jag älskar`

Exemplet ovan visar hur man extraherar en substräng från en given sträng. Här tar vi de 9 första tecknen från strängen "Jag älskar att programmera i Go."

## Fördjupning:

(1) Extrahering av substrängar har en lång historik inom programmeringsvärlden och används flitigt inom de flesta språk, inklusive Go. 

(2) Go erbjuder inbyggda metoder för att bearbeta strängar, men du kan även använda paket som "strings" eller "strconv" för mer komplexa uppgifter. 

(3) I Go, substrängar extraheras med hjälp av index. Språket antar en 0-baserad indexering, vilket innebär att det första tecknet i strängen är index 0.

## Se även:

1. Go's officiella dokumentation om strängar:  https://golang.org/pkg/strings/
2. Mer om Go's indexbaserade strängmanipulationer: https://gobyexample.com/string-functions
3. Utförlig guide om att arbeta med strängar i Go: https://www.callicoder.com/go-string/