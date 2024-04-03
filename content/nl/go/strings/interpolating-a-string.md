---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:27.563476-07:00
description: "Hoe te: In Go wordt stringinterpolatie vaak bereikt met behulp van het\
  \ `fmt`-pakket, met name met de functie `Sprintf`, waarmee je variabelen in een\u2026"
lastmod: '2024-03-13T22:44:50.273023-06:00'
model: gpt-4-0125-preview
summary: In Go wordt stringinterpolatie vaak bereikt met behulp van het `fmt`-pakket,
  met name met de functie `Sprintf`, waarmee je variabelen in een string kunt injecteren
  door opmaakvervoegwoorden te specificeren.
title: Een string interpoleren
weight: 8
---

## Hoe te:
In Go wordt stringinterpolatie vaak bereikt met behulp van het `fmt`-pakket, met name met de functie `Sprintf`, waarmee je variabelen in een string kunt injecteren door opmaakvervoegwoorden te specificeren. De vervoegwoorden zijn placeholders in de formatstring en worden vervangen door de waarden van de gegeven variabelen. Hier is hoe je het gebruikt:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Sprintf gebruiken voor stringinterpolatie
    message := fmt.Sprintf("Hallo, mijn naam is %s en ik ben %d jaar oud.", name, age)
    fmt.Println(message) // Uitvoer: Hallo, mijn naam is Jane en ik ben 28 jaar oud.
}
```

Merk op dat `%s` wordt gebruikt voor strings en `%d` voor gehele getallen. De documentatie van het `fmt`-pakket biedt een uitgebreide lijst van opmaakvervoegwoorden voor verschillende datatypen.

## Diepere Duik
Het concept van stringinterpolatie bestaat in veel programmeertalen, zij het met verschillende syntaxis en mogelijkheden. In Go, terwijl de `Sprintf`-functie van het `fmt`-pakket de meest gebruikte benadering is, is dit mogelijk niet altijd de meest efficiënte, vooral niet voor eenvoudige concatenaties of wanneer gewerkt wordt binnen zeer prestatiegevoelige code.

Het `fmt`-pakket gebruikt reflectie om dynamisch de typen van de variabelen op runtime te interpreteren, wat, hoewel flexibel, overhead met zich meebrengt. Voor scenario's waar prestatie cruciaal is, kunnen directe stringconcatenatie of het type `strings.Builder` betere alternatieven bieden. Directe concatenatie is eenvoudig, maar kan onhandelbaar worden met meerdere variabelen. `Strings.Builder`, aan de andere kant, biedt een meer prestatiegerichte en leesbare manier om complexe strings te bouwen in een lus of bij het omgaan met veel variabelen:

```go
var sb strings.Builder
sb.WriteString("Hallo, mijn naam is ")
sb.WriteString(name)
sb.WriteString(" en ik ben ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" jaar oud.")
message := sb.String()

fmt.Println(message) // Geeft hetzelfde uit als eerder
```

Uiteindelijk hangt de keuze tussen `fmt.Sprintf`, directe concatenatie en `strings.Builder` af van de specifieke vereisten van je applicatie, zoals de complexiteit van de te construeren string en prestatieoverwegingen.
