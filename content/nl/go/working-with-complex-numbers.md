---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:34.559150-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen, samengesteld uit een reëel en een imaginair deel (zoals 5 + 7i), zijn cruciaal in vakgebieden zoals techniek, fysica en signaalverwerking. Programmeurs werken ermee om problemen in deze domeinen op te lossen die lastig te kraken zouden zijn met enkel reële getallen.

## Hoe:
Go heeft ingebouwde ondersteuning voor complexe getallen. Hier is een snelle doorloop:

```go
package main

import (
	"fmt"
	"math/cmplx"
)

func main() {
	// Creëren van complexe getallen
	a := complex(2, 3)
	b := 4 + 5i

	// Basishandelingen
	fmt.Println("Optellen:", a+b)
	fmt.Println("Aftrekken:", a-b)
	fmt.Println("Vermenigvuldigen:", a*b)
	fmt.Println("Delen:", a/b)

	// Eigenschappen van complexe getallen
	fmt.Println("Reëel deel:", real(b))
	fmt.Println("Imaginair deel:", imag(b))
	fmt.Println("Toegevoegde:", cmplx.Conj(b))
	fmt.Println("Grootte:", cmplx.Abs(b))
	fmt.Println("Fasehoek (radians):", cmplx.Phase(b))
}

```

Voorbeelduitvoer:

```
Optellen: (6+8i)
Aftrekken: (-2-2i)
Vermenigvuldigen: (-7+22i)
Delen: (0.5609756097560976+0.0487804878048781i)
Reëel deel: 4
Imaginair deel: 5
Toegevoegde: (4-5i)
Grootte: 6.4031242374328485
Fasehoek (radians): 0.8960553845713439
```

## Diepere Duik
Een hele tijd terug werden complexe getallen met argwaan bekeken - sommigen dachten dat ze nutteloos waren! Na verloop van tijd werd hun kracht in het beschrijven van fysische fenomenen duidelijk. Ze zijn fundamenteel in de kwantumfysica, regeltechniek, en elektrotechniek, om maar een paar gebieden te noemen.

In Go worden complexe getallen gerepresenteerd met behulp van een gegevenstype genaamd `complex128` (64 bits voor zowel het reële als het imaginaire deel) of `complex64` (32 bits elk). Onder de motorkap zijn dit echt gewoon twee `float64`s of `float32`s aan elkaar vast. Go's standaardbibliotheek, `math/cmplx`, biedt functies voor complexe wiskundige operaties. Dit bespaart je de ingewikkelde wiskunde en stelt je in staat je te richten op het oplossen van problemen.

Alternatieven voor de ingebouwde ondersteuning van Go omvatten het gebruik van externe bibliotheken of het zelf afhandelen van complexe getallen. Maar deze zijn zelden nodig omdat de native ondersteuning van Go efficiënt en goed geïntegreerd is in de taal.

## Zie Ook
Bekijk deze links voor meer over Go’s mogelijkheden met complexe getallen:
- Go's officiële documentatie: https://golang.org/pkg/math/cmplx/
- Een diepgaande opfriscursus over complexe getallen: https://www.mathsisfun.com/numbers/complex-numbers.html
- Praktische toepassingen van complexe getallen in de techniek: https://ieeexplore.ieee.org/document/528dunno
