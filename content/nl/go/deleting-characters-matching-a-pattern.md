---
title:                "Karakters verwijderen die overeenkomen met een patroon"
date:                  2024-01-28T21:59:02.964964-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Karakters verwijderen die overeenkomen met een patroon gaat over het specifiek verwijderen van bepaalde stukjes uit een string—zoals onkruid wieden in je teksttuin. Programmeurs doen dit voor opschoning, opmaak of het parseren van gegevens, om ervoor te zorgen dat de tekst onberispelijk en nuttig is.

## Hoe:

In Go gebruiken we de pakketten `strings` en `regexp`. Hier is het insider verhaal met code:

```go
package main

import (
	"fmt"
	"regexp"
	"strings"
)
 
func main() {
	// Gebruikmakend van het strings pakket om een reeks karakters te verwijderen
	str1 := "Hallo, 123 Wereld!"
	cleanStr1 := strings.Map(func(r rune) rune {
		if r >= '0' && r <= '9' {
			return -1 // Verwijder karakter
		}
		return r // Behoud karakter
	}, str1)

	fmt.Println(cleanStr1) // Uitvoer: Hallo,  Wereld!

	// Gebruikmakend van het regexp pakket om karakters te verwijderen die overeenkomen met een patroon
	str2 := "Go 1.18 is de huidige versie!"
	re := regexp.MustCompile(`[0-9]+`)
	cleanStr2 := re.ReplaceAllString(str2, "")

	fmt.Println(cleanStr2) // Uitvoer: Go . is de huidige versie!
}
```

## Diepgaande Duik

Terug in de oude dagen, toen programmeertalen meer leken op arcanae spreuken, was patroonherkenning een begeerde vaardigheid. Regelmatige expressies (regex) waren het Zwitserse zakmes voor deze klus. Go heeft dit echter gemakkelijk en efficiënt gemaakt, door deze kracht te integreren met het `regexp` pakket.

Nu, waarom zou je niet gewoon `strings.Replace` of `strings.ReplaceAll` gebruiken? Nou, die zijn prima voor simpele, statische vervangingen. Maar wanneer je patronen wild zijn als lianen in de jungle, is regex waar je naar omkijkt.

Onder de motorkap compiles `regexp` een patroon in een toestandsmachine. Elk karakter wordt gecontroleerd tegen deze machine en overeenkomsten worden gewied. Dit betekent zwaar tillen bij de eerste compiler, maar bliksemsnel daarna.

Alternatieve methoden? Je hebt `bytes.Buffer` voor het bouwen van strings zonder patronen en `strings.Builder` in nieuwere versies voor als je allergisch bent voor onnodige toewijzingen.

## Zie Ook

De plaatsen om je kennis verder te verdiepen:
- Go by Example: Regular Expressions - https://gobyexample.com/regular-expressions
- Go Doc: Pakket strings - https://pkg.go.dev/strings
- Go Doc: Pakket regexp - https://pkg.go.dev/regexp
- Regular Expression Playground - https://regex101.com/ (Niet specifiek voor Go, maar superhandig)
