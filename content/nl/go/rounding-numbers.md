---
title:                "Afronden van getallen"
date:                  2024-01-28T22:06:43.075248-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Afronden van getallen betekent een getal aanpassen naar het dichtstbijzijnde gehele getal of gespecificeerde decimale plaats. Het wordt gedaan om waarden te vereenvoudigen, ze beter leesbaar te maken, of om ze in bepaalde beperkingen te laten passen, zoals bij het werken met valuta.

## Hoe te:
Go's `math` pakket is je vriend voor afronden. Gebruik `math.Round`, `math.Floor` en `math.Ceil` voor eenvoud:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	getal := 3.14159
	fmt.Println("Rond af:", math.Round(getal))  // Afronden naar het dichtstbijzijnde hele getal
	fmt.Println("Floor:", math.Floor(getal)) // Naar beneden afronden
	fmt.Println("Ceil: ", math.Ceil(getal))  // Naar boven afronden
}
```

Voorbeelduitvoer:
```
Rond af: 3
Floor: 3
Ceil: 4
```

Voor specifieke decimale plaatsen, vermenigvuldig, rond af, en deel vervolgens:

```go
func afrondenOpDecimalePlaats(getal float64, decimalePlaatsen int) float64 {
	shift := math.Pow(10, float64(decimalePlaatsen))
	return math.Round(getal*shift) / shift
}

func main() {
	getal := 3.14159
	fmt.Println("Afgerond op 2 decimale plaatsen:", afrondenOpDecimalePlaats(getal, 2))
}
```

Voorbeelduitvoer:
```
Afgerond op 2 decimale plaatsen: 3.14
```

## Diepgaand
Het afronden van getallen is niet nieuw - het gaat terug tot de oude wiskunde, altijd met het doel om te vereenvoudigen. De `math.Round` in Go gebruikt [bankiers afronding](https://nl.wikipedia.org/wiki/Afronding#Naar_het_dichtstbijzijnde_even_getal), wat betekent dat 0,5 afgerond wordt naar het dichtstbijzijnde even getal, waardoor een bias wordt verminderd die sommen zou kunnen beïnvloeden.

Drijvende-kommagetallen kunnen lastig zijn vanwege hun binaire representatie, die mogelijk niet alle decimalen exact vertegenwoordigt. De aanpak van Go behoudt echter de meeste tijd het verwachte gedrag.

Er bestaan andere afrondingsmethoden, zoals "afronden naar boven" of "afronden van nul af", maar Go's standaardbibliotheek is wat direct beschikbaar is. Voor meer complexe behoeften heb je misschien een bibliotheek van een derde partij nodig of moet je je eigen oplossing bedenken.

## Zie ook
- Go's `math` pakket: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- IEEE 754 standaard voor zwevendekommagetallen (de basis van Go voor het omgaan met zwevende punten): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Begrip van zwevendekommagetallen: ["Wat elke informaticus zou moeten weten over zwevendekommagetallen"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
