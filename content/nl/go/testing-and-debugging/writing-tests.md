---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:55.822608-07:00
description: "Tests schrijven in Go betekent kleine, beheersbare stukken code cre\xEB\
  ren die de functionaliteit en het gedrag van je applicatie valideren. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.291220-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven in Go betekent kleine, beheersbare stukken code cre\xEB\
  ren die de functionaliteit en het gedrag van je applicatie valideren. Programmeurs\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven in Go betekent kleine, beheersbare stukken code creëren die de functionaliteit en het gedrag van je applicatie valideren. Programmeurs schrijven tests om ervoor te zorgen dat hun code zoals verwacht werkt onder verschillende omstandigheden, om refactoring te faciliteren en om regressies te helpen voorkomen.

## Hoe te:

In Go worden tests doorgaans geschreven in hetzelfde pakket als de code die ze testen. Bestanden met tests krijgen de `_test.go` suffix. Tests zijn functies die een pointer naar het testing.T object (uit het `testing` pakket) als argument nemen, en ze signaleren falen door methoden aan te roepen zoals `t.Fail()`, `t.Errorf()`, enz.

Voorbeeld van een eenvoudige test voor een functie `Add` gedefinieerd in `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Testbestand `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    resultaat := Add(1, 2)
    verwacht := 3
    if resultaat != verwacht {
        t.Errorf("Add(1, 2) = %d; wil %d", resultaat, verwacht)
    }
}
```

Voer je tests uit met het commando `go test` in dezelfde directory als je testbestanden. Voorbeelduitvoer die aangeeft dat een test slaagt, zou er als volgt uitzien:

```
PASS
ok      example.com/my/math 0.002s
```

Voor tabelgestuurde tests, die je in staat stellen om efficiënt verschillende invoer- en uitvoercombinaties te testen, definieer je een slice van structs die testgevallen vertegenwoordigen:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        verwacht int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testnaam := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testnaam, functie(t *testing.T) {
            antw := Add(tt.x, tt.y)
            if antw != tt.verwacht {
                t.Errorf("kreeg %d, wil %d", antw, tt.verwacht)
            }
        })
    }
}
```

## Diepgaand

Het Go-testframework, geïntroduceerd in Go 1 samen met de taal zelf, was ontworpen om naadloos te integreren met de Go toolchain, wat Go's nadruk op eenvoud en efficiëntie in de softwareontwikkeling weerspiegelt. In tegenstelling tot sommige testframeworks in andere talen die afhankelijk zijn van externe bibliotheken of complexe opstellingen, biedt Go's ingebouwde `testing` pakket een eenvoudige manier om tests te schrijven en uit te voeren.

Een interessant aspect van Go's benadering van testen is het principe van conventie boven configuratie dat het aanneemt, zoals het bestandsbenamingspatroon (`_test.go`) en het gebruik van standaardbibliotheekfunctionaliteiten boven externe afhankelijkheden. Deze minimalistische aanpak moedigt ontwikkelaars aan om tests te schrijven, aangezien de instapdrempel laag is.

Hoewel Go's ingebouwde testfaciliteiten veel dekken, zijn er scenario's waarin externe tools of frameworks meer functionaliteiten kunnen bieden, zoals mock-generatie, fuzz testing, of behavior-driven development (BDD) stijltests. Populaire bibliotheken zoals Testify of GoMock vullen de standaardtestmogelijkheden van Go aan, en bieden meer expressieve beweringen of mogelijkheden voor mock-generatie, die met name nuttig kunnen zijn in complexe applicaties met veel afhankelijkheden.

Ondanks het bestaan van deze alternatieven blijft het standaard Go-testpakket de hoeksteen voor testen in Go vanwege de eenvoud, prestaties, en strakke integratie met de taal en toolchain. Of ontwikkelaars het nu aanvullen met externe tools of niet, het Go-testframework biedt een solide basis voor het waarborgen van codekwaliteit en betrouwbaarheid.
