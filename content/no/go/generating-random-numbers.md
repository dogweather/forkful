---
title:                "Go: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Å generere tilfeldige tall er en viktig del av mange programmer og applikasjoner. Tilfeldige tall kan brukes til å simulere utvikling, lage tilfeldige spill eller generere unike nøkler. I denne bloggposten vil vi se nærmere på hvordan du kan generere tilfeldige tall med Go-programmeringsspråket.

# Hvordan

For å generere tilfeldige tall i Go, kan vi bruke pakken "math/rand". Denne pakken har en funksjon kalt "Intn" som genererer et tilfeldig heltall mellom 0 og det spesifiserte tallet. Her er et enkelt eksempel på hvordan du kan bruke denne funksjonen:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	// Generer et tilfeldig tall mellom 0 og 10
	randomNumber := rand.Intn(10)
	fmt.Println("Tilfeldig tall:", randomNumber)
}
```

Dette vil gi følgende output:

```
Tilfeldig tall: 7
```

Vi kan også generere flere tilfeldige tall ved hjelp av en løkke. Her er et eksempel på hvordan du kan generere 5 tilfeldige tall mellom 0 og 100 og lagre dem i en liste:

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	// Lager en tom liste for tilfeldige tall
	var randomNumbers []int

	// Genererer fem tilfeldige tall og legger dem til i listen
	for i := 0; i < 5; i++ {
		randomNumber := rand.Intn(100)
		randomNumbers = append(randomNumbers, randomNumber)
	}

	// Skriver ut listen med tilfeldige tall
	fmt.Println("Tilfeldige tall:", randomNumbers)
}
```

Dette vil gi følgende output:

```
Tilfeldige tall: [44 82 10 16 27]
```

# Deep Dive

Når vi bruker funksjonen "Intn" for å generere tilfeldige tall, vil tallene som blir generert være basert på en seed-verdi. Dette betyr at dersom vi kjører programmet vårt flere ganger, vil vi få de samme tilfeldige tallene hver gang. For å unngå dette kan vi endre seed-verdien til "rand" ved hjelp av funksjonen "Seend".

```Go
// Endrer seed-verdien til basert på tiden, slik at vi får forskjellige tilfeldige tall
rand.Seed(time.Now().UnixNano())
```

Vi kan også begrense området til de tilfeldige tallene ved å bruke funksjonen "Intn"'s parameter. For eksempel, for å generere tilfeldige tall mellom 50 og 100, kan vi bruke følgende kode:

```Go
rand.Intn(51) + 50
```

Dette vil generere et tilfeldig tall mellom 50 og 100.

# Se også

- Offisiell Go-dokumentasjon: https://golang.org/pkg/math/rand/
- Tutorial om tilfeldige tall i Go: https://www.youtube.com/watch?v=u80i4k0jPac
- Blogginnlegg om å generere tilfeldige tall i Go: https://blog.xmh.io/posts/go-seeding-math-rand-intn/