---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:49:18.551190-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall betyr å skape nummer som ikke kan forutsies, og det er viktig for alt fra spill til sikkerhet. Kode trenger ofte element av uforutsigbarhet for å teste scenarier eller gi en følelse av naturlighet.

## Slik gjør du:
For å bruke Go's innebygde funksjoner for å generere tilfeldige tall, trenger du først å importere "math/rand"-pakken og "time"-pakken for å bruke tidsavhengig seeding. Her er et eksempel:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Seed generator, bruk nåværende tid
	rand.Seed(time.Now().UnixNano())

	// Generer et tilfeldig heltall mellom 0 og 99
	randomNumber := rand.Intn(100)
	fmt.Println("Tilfeldig tall: ", randomNumber)

	// Generer et tilfeldig flyttall mellom 0.0 og 1.0
	randomFloat := rand.Float64()
	fmt.Println("Tilfeldig flyttall: ", randomFloat)
}
```

Når du kjører programmet, får du noe slikt:

```
Tilfeldig tall: 47
Tilfeldig flyttall: 0.8223541256379213
```

Resultatene vil variere hver gang siden de er tilfeldige.

## Dypdykk
Historisk har programmerere brukt ulike metoder for å generere pseudo-tilfeldige tall, som egentlig er deterministiske, men ser tilfeldige ut. Go bruker én av flere mulige algoritmer kalt 'pseudorandom number generators' (PRNGs). Seed-funksjonen er kritisk her; uten en variabel seed, vil programmet gi de samme resultatene hver gang det kjøres.

Alternativer for Go inkluderer å bruke kryptografisk sikre tall fra "crypto/rand"-pakken, nyttig når du trenger høyere sikkerhet, så som for passord eller krypteringsnøkler.

Implementeringsmessig, "math/rand"-pakken er bra for grunnleggende tilfeldighet og testing, mens "crypto/rand" er tregere, men gir bedre tilfeldighet.

## Se Også
- Golang dokumentasjon for 'math/rand'-pakken: https://golang.org/pkg/math/rand/
- Golang blogg om tilfeldighet: https://blog.golang.org/random
- "crypto/rand"-dokumentasjon: https://golang.org/pkg/crypto/rand/
