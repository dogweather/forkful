---
title:                "Een string omzetten naar kleine letters"
date:                  2024-01-28T21:57:36.585637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een string naar kleine letters omzetten betekent het transformeren van alle alfabetische tekens in de tekst naar hun equivalent in kleine letters. Programmeurs doen dit voor consistentie, vooral bij hoofdletterongevoelige vergelijkingen, datanormalisatie, en om dubbele invoeren die alleen in hoofdlettergebruik verschillen, te voorkomen.

## Hoe te:
In Go, gebruik `strings.ToLower` om een string naar kleine letters om te zetten. Zo doe je dat:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Hello, World!"
	lower := strings.ToLower(original)
	fmt.Println(lower) // Uitvoer: hello, world!
}
```
Voer de code uit. De uitvoer is de versie van de string in kleine letters.

## Diepere duik
Het concept van lettergeval omzetting bestaat al zolang er hoofdletters en kleine letters bestaan. Go handelt dit af met het `strings` pakket, dat een eenvoudige, efficiënte manier biedt om strings te transformeren.

Alternatieven? Zeker. Je zou over elk karakter kunnen itereren en handmatig zijn lettergeval kunnen controleren, maar waarom zou je het wiel opnieuw uitvinden?

Wat betreft de implementatie is `ToLower` ingewikkelder onder de motorkap dan het lijkt. Het is zich bewust van Unicode en hanteert correct karakters buiten de basis ASCII-set. Dit betekent dat het karakters naar kleine letters zal omzetten vanuit het Grieks, Cyrillisch, enz., niet alleen het Engelse alfabet.

## Zie Ook
Voor meer, bekijk:
- De Go `strings` pakketdocumentatie: https://pkg.go.dev/strings
- De Unicode Standaard: https://www.unicode.org/standard/standard.html
- Go bij Voorbeeld: Strings - https://gobyexample.com/strings
