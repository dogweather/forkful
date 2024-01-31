---
title:                "Tests Schrijven"
date:                  2024-01-28T22:13:01.628676-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven betekent code ontwerpen om te controleren of andere code werkt. Programmeurs doen dit om bugs vroeg te ontdekken, functionaliteit te waarborgen en toekomstige problemen te voorkomen.

## Hoe:

Go heeft een ingebouwd testpakket genaamd `testing`. Stel je voor, je hebt een functie `Add` die twee ints optelt:

```Go
// add.go
package math

func Add(x, y int) int {
    return x + y
}
```

Schrijf een test als volgt:

```Go
// add_test.go
package math

import (
    "testing"
)

func TestAdd(t *testing.T) {
    resultaat := Add(1, 2)
    verwacht := 3
    if resultaat != verwacht {
        t.Errorf("Add(1, 2) = %d; wil %d", resultaat, verwacht)
    }
}
```

Voer tests uit met `go test`. Je zult output zien zoals:

```
PASS
ok      example.com/your-module/math   0.002s
```

## Diepe Duik

Go introduceerde ingebouwde tests in 2011. Het is eenvoudiger dan het gebruiken van een aparte bibliotheek. Je schrijft tests in `_test.go` bestanden, met gebruik van `testing.T` om fouten te rapporteren.

Alternatieven? Zeker, je kunt Testify gebruiken voor beweringen, Ginkgo voor BDD, of GoCheck voor meer geavanceerde functies. Maar het `testing` pakket is zonder afhankelijkheden, eenvoudig en vaak genoeg.

Achter de schermen compileert `go test` je code en tests samen, voert ze uit en rapporteert resultaten. Het is idiomatisch Go: algemene gevallen eenvoudig, speciale gevallen mogelijk.

## Zie Ook

Voor extra's, bekijk de documentatie:

- Testpakket: [https://pkg.go.dev/testing](https://pkg.go.dev/testing)
- Tabelgestuurde tests: [https://github.com/golang/go/wiki/TableDrivenTests](https://github.com/golang/go/wiki/TableDrivenTests)
