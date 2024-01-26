---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:02.300116-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Losowe liczby to podstawa symulacji, gier i bezpieczeństwa. Programiści je generują, aby dodać nieprzewidywalność i fałszywą entropię do swoich aplikacji.

## How to: (Jak to zrobić:)
```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// Zainicjalizuj generator liczb losowych
	rand.Seed(time.Now().UnixNano())

	// Generuj losową liczbę z zakresu od 0 do 99
	randomNumber := rand.Intn(100)
	fmt.Println(randomNumber)

	// Generuj losowy float z zakresu od 0 do 1
	randomFloat := rand.Float64()
	fmt.Println(randomFloat)
}
```
**Wyjście przykładowe:**
```
42
0.8125720833213298
```

## Deep Dive (W Glebi Tematu)
Generowanie liczb losowych w informatyce sięga lat 40. XX wieku. W Go używamy pakietu `math/rand` do pseudo-losowości, ale dla bardziej krytycznych zastosowań, jak kryptografia, sięgamy po `crypto/rand`, który jest bezpieczniejszy. Losowość w `math/rand` opiera się na nasionie (seed), determinującym szereg generowanych wartości; zmieniaj nasiono, żeby otrzymać różne sekwencje.

## See Also (Zobacz Także)
- [math/rand - Pakiet Go](https://pkg.go.dev/math/rand)
- [crypto/rand - Pakiet Go](https://pkg.go.dev/crypto/rand)
- [Nasiona i generator pseudolosowy - Wikipedia](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych)
