---
title:                "Refactoring"
date:                  2024-01-28T22:06:09.772242-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercode zonder het externe gedrag ervan te veranderen. Programmeurs doen dit om niet-functionele attributen van de software, zoals leesbaarheid en onderhoudbaarheid, te verbeteren, wat de code makkelijker te begrijpen maakt, de complexiteit vermindert en helpt fouten makkelijker te vinden.

## Hoe:
Laten we duiken in een eenvoudig voorbeeld van Go code refactoring. We nemen een fragment dat het gemiddelde berekent van een reeks getallen en refactoren dit voor duidelijkheid en herbruikbaarheid.

Originele code:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Gemiddelde:", average)
}
```

Gerefactorde code:
```Go
package main

import "fmt"

// CalculateAverage neemt een reeks van float64 en retourneert het gemiddelde.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Gemiddelde:", average)
}
```

In de gerefactorde code hebben we de logica die het gemiddelde berekent geëxtraheerd naar een aparte functie met de naam `CalculateAverage`. Dit maakt de `main` functie beknopter en de logica voor de gemiddeldeberekening herbruikbaar en testbaar.

## Diepere Duik
Refactoring van code is geen modern concept; het bestaat al langer dan het wijdverbreide gebruik van computers. De praktijk is waarschijnlijk begonnen in het domein van de werktuigbouwkunde of zelfs eerder. In software werd het meer formeel met de komst van objectgeoriënteerd programmeren en extreme programming (XP) in de jaren '90, met name beïnvloed door het baanbrekende boek van Martin Fowler "Refactoring: Improving the Design of Existing Code."

Er zijn talloze refactoringtechnieken, van eenvoudig hernoemen van variabelen voor duidelijkheid tot complexere patronen zoals het extraheren van methoden of klassen. Het belangrijkste is om kleine, incrementele veranderingen aan te brengen die de functionaliteit van de software niet wijzigen maar de interne structuur verbeteren.

Bij het gebruik van Go kan refactoring eenvoudig zijn vanwege de eenvoud van de taal en de krachtige standaardbibliotheek. Het is echter nog steeds belangrijk om een goede set unit tests te hebben om ervoor te zorgen dat refactoring geen bugs introduceert. Hulpmiddelen zoals `gorename` en `gofmt` helpen een deel van het proces te automatiseren en IDE's hebben vaak ingebouwde refactoringondersteuning.

Naast handmatige refactoring zijn er ook enkele geautomatiseerde code refactoring tools beschikbaar voor Go, zoals GoLand's refactoringtools en Go Refactor. Hoewel deze het proces kunnen versnellen, zijn ze geen vervanging voor het begrijpen van de code en het maken van overwogen wijzigingen.

## Zie Ook
 - [Refactoring in Go: Eenvoudig is Mooi](https://go.dev/blog/slices)
 - [Effectief Go: Refactoring met Interfaces](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowler's Refactoring Pagina](https://refactoring.com/)
 - [GoLand Refactoring Tools](https://www.jetbrains.com/go/features/refactorings/)
