---
title:                "Avrunding av tall"
date:                  2024-01-26T03:45:03.094600-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å avrunde tall betyr å justere et tall til nærmeste hele tall eller spesifiserte desimalplass. Det gjøres for å forenkle verdier, gjøre dem mer lesbare, eller få dem til å passe innenfor visse begrensninger, som for eksempel når man jobber med valutaer.

## Hvordan:
Go sin `math` pakke er din venn for avrunding. Bruk `math.Round`, `math.Floor`, og `math.Ceil` for enkelthet:

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	number := 3.14159
	fmt.Println("Round:", math.Round(number))  // Avrund til nærmeste hele tall
	fmt.Println("Floor:", math.Floor(number)) // Avrund ned
	fmt.Println("Ceil: ", math.Ceil(number))  // Avrund opp
}
```

Eksempel på utdata:
```
Round: 3
Floor: 3
Ceil: 4
```

For spesifikke desimalplasser, multipliser, avrund, deretter del:

```go
func roundToDecimalPlace(number float64, decimalPlaces int) float64 {
	shift := math.Pow(10, float64(decimalPlaces))
	return math.Round(number*shift) / shift
}

func main() {
	number := 3.14159
	fmt.Println("Avrundet til 2 desimalplasser:", roundToDecimalPlace(number, 2))
}
```

Eksempel på utdata:
```
Avrundet til 2 desimalplasser: 3.14
```

## Dypdykk
Avrunding av tall er ikke nytt—det går tilbake til antikkens matematikk, alltid med mål om enkelhet. `math.Round` i Go bruker [bankers avrunding](https://en.wikipedia.org/wiki/Rounding#Round_half_to_even), noe som betyr at 0,5 avrundes til nærmeste partall, dette reduserer en skjevhet som kunne påvirke summer.

Flyttall kan være knepige på grunn av deres binære representasjon, som kanskje ikke nøyaktig representerer alle desimaler. Go sin tilnærming opprettholder imidlertid forventet oppførsel det meste av tiden.

Andre avrundingsmetoder eksisterer, som "avrund halv opp" eller "avrund halv bort fra null", men Go sin standardbibliotek er det som er lett tilgjengelig. For mer komplekse behov, kan du trenge et tredjepartsbibliotek eller lage din egen løsning.

## Se Også
- Go sin `math` pakke: [https://pkg.go.dev/math](https://pkg.go.dev/math)
- IEEE 754-standard for flyttallsaritmetikk (Gos grunnlag for håndtering av flyttall): [https://ieeexplore.ieee.org/document/4610935](https://ieeexplore.ieee.org/document/4610935)
- Forstå flyttall: ["What Every Computer Scientist Should Know About Floating-Point Arithmetic"](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
