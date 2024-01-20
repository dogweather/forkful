---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er en prosess for å produsere tall på en uforutsigbar måte. Programmere gjør dette for å skape uforutsigbarhet, simulere fenomener, eller prototypere algoritmer.

## Hvordan:
I Go kan du generere tilfeldige tall ved hjelp av `math/rand` biblioteket:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println(rand.Intn(100))
}
```
Kjører du dette programmet, vil du få forskjellig output hver gang siden tiden hvorfra tallet blir seedet er alltid forskjellig

## Dypdykk
Bruken av tilfeldig tallgenerering er historisk viktig innen programmering, og daterer tilbake til de tidligste dagene av computing for simuleringer. 

Det finnes også andre metoder for generering av tilfeldige tall i Go, som bruk av `crypto/rand` biblioteket for kryptografisk sikre tall. 

Intern fungering av random nummer generasjon i Go bruker en pseudo-random nummer generator som er seedet ved programstart. Dette gir en unik sekvens av numre hver gang programmet kjøres.

## Se også:
Her er noen nyttige lenker med mer informasjon:
- Godocs for `math/rand` biblioteket: https://golang.org/pkg/math/rand/
- Introduksjon til Random Number Generator (RNG) i Go: https://yourbasic.org/golang/generate-random-number/ 
- Generering av kryptografisk sikre tilfeldige tall: https://go.dev/play/p/3i0wGlUFuC.tgz