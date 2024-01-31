---
title:                "Een string interpoleren"
date:                  2024-01-28T22:01:55.924093-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

String interpolatie maakt het mogelijk om variabelen in te bedden in strings. Het is handig voor het creëren van berichten, het formatteren van gegevens en het bouwen van SQL-query's zonder veel plussen en aanhalingstekens.

## Hoe dan:

In Go gebruik je het `fmt` pakket voor string interpolatie.

```Go
package main

import (
    "fmt"
)

func main() {
    name := "Morgan"
    age := 28
    message := fmt.Sprintf("Hoi, mijn naam is %s en ik ben %d jaar oud.", name, age)
    fmt.Println(message)
}

// Uitvoer: Hoi, mijn naam is Morgan en ik ben 28 jaar oud.
```

Gebruik `%s` voor strings, `%d` voor gehele getallen, `%f` voor zwevende kommagetallen. Er zijn meer werkwoorden voor andere types.

## Diepere Duik

String interpolatie is een kernfunctie geweest in veel talen—Python, Ruby, en meer. In Go maakt het niet echt deel uit van de taal zelf, maar wordt het geleverd via het `fmt` pakket. Deze benadering geeft je een betere controle en veiligheid, vooral met type-specifieke werkwoorden.

Alternatieven? Ja—naast `fmt.Sprintf`, is er `fmt.Fprintf` om naar elke writer te schrijven, en `fmt.Printf` om direct af te drukken. Voor Go 1.10 dagen zagen mensen het aan elkaar plakken van strings met `+` of het gebruik van `bytes.Buffer`. Deze zijn nog steeds geldig, maar minder handig.

Implementatie details? Het `fmt` pakket gebruikt reflectie om formattering te hanteren op basis van de werkwoorden en het type variabele. Het is efficiënt, maar onthoud dat het gebruik van het verkeerde werkwoord voor een type kan leiden tot runtime fouten.

## Zie Ook

- Documentatie van Go's `fmt` pakket: https://pkg.go.dev/fmt
- Go by Example’s visie op string formattering: https://gobyexample.com/string-formatting
- Een blogpost over strategieën voor string concatenatie in Go: https://blog.golang.org/strings
