---
title:                "Willekeurige getallen genereren"
date:                  2024-01-28T22:01:12.994105-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in Go omvat het gebruik van het `math/rand`-pakket om pseudo-willekeurige getallen te produceren voor verschillende toepassingen, zoals het simuleren van experimenten, het genereren van testgegevens, of het toevoegen van onvoorspelbaarheid aan spellen. Programmeurs gebruiken deze functie om dynamischere en minder voorspelbare softwaregedragingen te creëren.

## Hoe te:

Om te beginnen met het genereren van willekeurige getallen in Go, moet je het `math/rand`-pakket en het `time`-pakket importeren om de generator van willekeurige getallen te zaaien voor meer onvoorspelbaarheid. Hier is een basisvoorbeeld:

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// De generator zaaien
	rand.Seed(time.Now().UnixNano())
	
	// Genereer een willekeurig geheel getal tussen 0 en 99
	randomInt := rand.Intn(100)
	fmt.Println("Willekeurig Geheel Getal:", randomInt)
	
	// Genereer een willekeurige float tussen 0.0 en 1.0
	randomFloat := rand.Float64()
	fmt.Println("Willekeurige Float:", randomFloat)
}
```

Een voorbeelduitvoer zou kunnen zijn:

```
Willekeurig Geheel Getal: 42
Willekeurige Float: 0.7304601899194229
```

Onthoud dat elke uitvoering verschillende getallen produceert vanwege het zaaien met de huidige tijd.

## Diepgaande Duik

Het `math/rand`-pakket in Go implementeert pseudo-willekeurige getalgeneratoren (PRNG's) voor verschillende distributies. Hoewel vrij effectief voor veel toepassingen, is het cruciaal om op te merken dat de door `math/rand` gegenereerde getallen niet geschikt zijn voor cryptografische doeleinden vanwege hun deterministische aard. Voor cryptografische behoeften is het `crypto/rand`-pakket de juiste keuze; het biedt een veilige willekeurige getalgenerator.

De implementatie van `math/rand` is gebaseerd op een subtractieve willekeurige getalgeneratoralgoritme, dat efficiënt is en een relatief lange periode heeft voordat sequenties zich herhalen. Echter, voor toepassingen die echt willekeurige sequenties vereisen, zoals cryptografische operaties, worden hardware willekeurige getalgeneratoren (RNG's) of het `crypto/rand`-pakket, dat interfaces biedt met systeemspecifieke veilige willekeurbronnen, aanbevolen.

`math/rand` maakt het mogelijk om te zaaien om variabiliteit te introduceren, maar hetzelfde zaad zal altijd dezelfde reeks getallen genereren, wat de deterministische aard van zijn willekeurigheid benadrukt. Dit maakt het geschikt voor simulaties of spellen waar reproduceerbaarheid wenselijk kan zijn voor debuggen of testdoeleinden.
