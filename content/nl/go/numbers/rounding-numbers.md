---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:48.746842-07:00
description: "Rond getallen af is het aanpassen van de waarde van een getal naar het\
  \ dichtstbijzijnde gehele getal of naar een specifiek aantal decimalen. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.282117-06:00'
model: gpt-4-0125-preview
summary: "Rond getallen af is het aanpassen van de waarde van een getal naar het dichtstbijzijnde\
  \ gehele getal of naar een specifiek aantal decimalen. Programmeurs\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Rond getallen af is het aanpassen van de waarde van een getal naar het dichtstbijzijnde gehele getal of naar een specifiek aantal decimalen. Programmeurs doen dit om redenen zoals het verbeteren van de leesbaarheid, het vereenvoudigen van berekeningen of om te voldoen aan domeinspecifieke precisie-eisen.

## Hoe:

In Go is er geen ingebouwde functie die direct getallen afrondt naar een specifiek aantal decimalen in het wiskundepakket. Je kunt echter afronden bereiken door een combinatie van functies voor gehele getallen of door een aangepaste functie voor decimalen te implementeren.

### Afronden naar het dichtstbijzijnde gehele getal:

Om af te ronden naar het dichtstbijzijnde gehele getal, kun je de `math.Floor()` functie gebruiken met toegevoegd 0,5 voor positieve getallen, en `math.Ceil()` min 0,5 voor negatieve getallen, afhankelijk van de richting waarin je wilt afronden.

```go
package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(math.Floor(3.75 + 0.5))  // Geeft uit: 4
	fmt.Println(math.Ceil(-3.75 - 0.5)) // Geeft uit: -4
}
```

### Afronden naar een specifiek aantal decimalen:

Voor het afronden naar een specifiek aantal decimalen kan een aangepaste functie worden gebruikt waarbij je het getal vermenigvuldigt met 10^n (waarbij n het aantal decimalen is), het dan naar het dichtstbijzijnde gehele getal afrondt zoals eerder, en vervolgens deelt door 10^n.

```go
package main

import (
	"fmt"
	"math"
)

func roundToDecimalPlace(number float64, places int) float64 {
	shift := math.Pow(10, float64(places))
	return math.Round(number*shift) / shift
}

func main() {
	fmt.Println(roundToDecimalPlace(3.14159, 2)) // Geeft uit: 3.14
	fmt.Println(roundToDecimalPlace(-3.14159, 3)) // Geeft uit: -3.142
}
```

## Diepgaand

Getallen afronden is een fundamentele bewerking in computerprogrammering, gekoppeld aan de historische uitdaging van het vertegenwoordigen van reële getallen in een binair systeem. De behoefte aan afronding ontstaat door het feit dat veel reële getallen niet precies in binaire vorm kunnen worden weergegeven, wat leidt tot benaderingsfouten.

In Go is de benadering tot afronden enigszins handmatig in vergelijking met talen die ingebouwde afrondfuncties bieden voor specifieke decimalen. Desalniettemin biedt het standaard `math` pakket van de Go-bibliotheek de basisbouwstenen (zoals `math.Floor` en `math.Ceil`) om elk gewenst afrondingsmechanisme te construeren.

Deze handmatige benadering, hoewel schijnbaar omslachtiger, biedt programmeurs een fijnere controle over hoe getallen worden afgerond, tegemoetkomend aan de precisie- en nauwkeurigheidseisen van verschillende toepassingen. Alternatieven zoals externe bibliotheken of het ontwerpen van aangepaste afrondingsfuncties kunnen eenvoudiger oplossingen bieden bij het omgaan met complexe getallen of voor meer geavanceerde wiskundige operaties die niet door de standaardbibliotheek worden gedekt.

Conclusie: hoewel de standaardbibliotheek van Go misschien geen directe afrondingsfunctionaliteit voor decimalen biedt, stelt het uitgebreide aanbod aan wiskundige functies ontwikkelaars in staat om robuuste afrondingsoplossingen te implementeren die zijn afgestemd op hun specifieke behoeften.
