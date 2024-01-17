---
title:                "Konvertera en sträng till små bokstäver"
html_title:           "Go: Konvertera en sträng till små bokstäver"
simple_title:         "Konvertera en sträng till små bokstäver"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Konvertering av en sträng till små bokstäver är en vanlig användning av programmeringsspråket Go. Detta innebär att man omvandlar alla bokstäver i en sträng till deras motsvarande små bokstäver, oavsett om bokstäverna är stora eller små från början. Detta kan vara användbart för att enhetliga hantering av text, för att jämföra textsträngar eller för att bara förbättra läsbarheten.

## Så här gör du:
```
Go package main
import "strings"
func main() {
	fmt.Println(strings.ToLower("HELLO WORLD"))
}
```
Detta skulle ge utskrift: "hello world". Vi importerar paketet "strings" för att ha tillgång till dess funktion för att konvertera en sträng till små bokstäver. Sedan använder vi funktionen "ToLower" och ger den som argument den sträng vi vill konvertera. Vi kan också använda denna funktion för att konvertera en variabel istället för en hårdkodad sträng.

## Djupdykning:
Konvertering av en sträng till små bokstäver är en vanlig operation i många programmeringsspråk, inte bara Go. Detta kallas ibland också för "lågering" eller "normalisering" av en textsträng. I Go finns det också en inbyggd funktion "ToLower" som konverterar en sträng till små bokstäver, men det finns också en alternativ funktion "ToLowerSpecial" som ger möjlighet att specificera en "lokal" (locale) för strängen. Detta kan vara användbart om strängen innehåller speciella tecken eller bokstäver som inte följer standard ASCII-regler.

## Se även:
- [Go Strings Package](https://golang.org/pkg/strings/)
- [Converting Strings to Lowercase in Go](https://www.digitalocean.com/community/tutorials/how-to-convert-strings-to-lowercase-in-go)