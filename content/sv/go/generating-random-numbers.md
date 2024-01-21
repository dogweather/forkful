---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:49:05.986246-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Slumpmässiga tal används för att skapa oförutsägbarhet i program och kan vara nyckeln i allt från spel till säkerhetsalgoritmer. Programmerare behöver dem för att testa, simulera och skapa dynamiska upplevelser.

## How to:
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Seed generatorn med nuvarande tid för variation
	rand.Seed(time.Now().UnixNano())

	// Skapa ett slumpmässigt tal mellan 0 och 99
	randomNumber := rand.Intn(100)
	fmt.Println(randomNumber)

	// Skapa ett slumpmässigt flyttal mellan 0.0 och 1.0
	randomFloat := rand.Float64()
	fmt.Printf("%.2f\n", randomFloat)
}
```
Exempelutdata:
```
42
0.81
```

## Deep Dive:
Generering av slumpmässiga tal startade långt innan datorer, med verktyg som tärningar och lotthjul. I datorns värld används pseudoslumptalsgeneratorer (PRNG) som simulerar slumpmässighet, eftersom strikt slumpmässighet är svår att uppnå digitalt.

`math/rand` i Go är inte säker för kryptografisk användning för det. Kryptografiskt säkra tal får du från `crypto/rand`, som läser ur en entropikälla. 

Go använder ett deterministiskt system, så om du vill ha nya nummer varje gång måste du "seed:a" generatorn - vi gör detta med `time.Now().UnixNano()` för att få unix-tiden i nanosekunder som seed.

## See Also:
- Go dokumentation för `math/rand` paketet: https://golang.org/pkg/math/rand/
- Kryptografiskt säkra slumptal med `crypto/rand`: https://golang.org/pkg/crypto/rand/
- Om pseudoslumptal och dess användning: https://en.wikipedia.org/wiki/Pseudorandom_number_generator