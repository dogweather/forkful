---
title:                "Code organiseren in functies"
date:                  2024-01-28T22:03:00.965143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code organiseren in functies"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Code organiseren in functies gaat over het opsplitsen van je code in herbruikbare stukken. Het maakt je code schoner, makkelijker te lezen en eenvoudiger te debuggen.

## Hoe te:
Hier is een Go snippet die een blok code laat zien, gevolgd door een gerefactorde versie die functies gebruikt:

```go
package main

import "fmt"

func main() {
    // Voor: Inline code
    fmt.Println("Som uitrekenen...")
    totaal := 0
    for i := 1; i <= 10; i++ {
        totaal += i
    }
    fmt.Println("Totale som is:", totaal)

    // Na: Gebruikmakend van een functie
    fmt.Println("Som uitrekenen met behulp van een functie...")
    som := getSom(1, 10)
    fmt.Println("Totale som is:", som)
}

// Functie om som binnen een bereik te berekenen
func getSom(start, eind int) int {
    totaal := 0
    for i := start; i <= eind; i++ {
        totaal += i
    }
    return totaal
}
```

Voorbeelduitvoer voor zowel inline als op functies gebaseerde code zal hetzelfde zijn:

```
Som uitrekenen...
Totale som is: 55
Som uitrekenen met behulp van een functie...
Totale som is: 55
```

## Diepere Duik
Voor het concept van functies opkwam, was programmeren grotendeels procedureel, met code die van boven naar beneden liep. Naarmate programma's groeiden, wakkerde deze aanpak inefficiëntie en codeherhaling aan.

Talen introduceerden functies als een abstractiemechanisme. In Go, omvatten functies blokken van code met een specifieke taak, waarbij het DRY (Don't Repeat Yourself) principe wordt aangemoedigd. Ze accepteren parameters en kunnen resultaten retourneren.

Handige tips:
- Noem functies duidelijk; een goede naam legt uit wat een functie doet.
- Houd ze kort; als een functie te veel doet, splits het dan op.
- Functies kunnen meerdere waarden retourneren, benut dat voor foutafhandeling.
- Hogere-orde functies (functies die andere functies nemen of retourneren) zijn krachtige hulpmiddelen in Go.

Alternatieven voor functies zijn onder andere inline code (rommelig voor complexe taken) en objectmethoden (deel van het objectgeoriënteerde paradigma beschikbaar in Go via structs).

## Zie Ook
- [Go by Example: Functies](https://gobyexample.com/functions)
- [Effectief Go: Functie](https://golang.org/doc/effective_go#functions)
