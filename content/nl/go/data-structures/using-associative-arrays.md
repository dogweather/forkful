---
aliases:
- /nl/go/using-associative-arrays/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:49.487598-07:00
description: "Associatieve arrays, bekend als maps in Go, stellen je in staat om sleutel-waardeparen\
  \ op te slaan waarbij elke unieke sleutel naar een waarde wijst.\u2026"
lastmod: 2024-02-18 23:09:01.330225
model: gpt-4-0125-preview
summary: "Associatieve arrays, bekend als maps in Go, stellen je in staat om sleutel-waardeparen\
  \ op te slaan waarbij elke unieke sleutel naar een waarde wijst.\u2026"
title: Gebruik van associatieve arrays
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, bekend als maps in Go, stellen je in staat om sleutel-waardeparen op te slaan waarbij elke unieke sleutel naar een waarde wijst. Programmeurs gebruiken maps voor efficiënte gegevensopvraging, aanpassing en om een verzameling van elementen te onderhouden die snel toegankelijk zijn met unieke sleutels.

## Hoe te:

Het creëren en initialiseren van een map in Go kan op verschillende manieren worden gedaan. Hier is een eenvoudig voorbeeld om je op weg te helpen:

```go
package main

import "fmt"

func main() {
    // Verklaren en initialiseren van een map
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Uitvoer: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

Om elementen toe te voegen of bij te werken, wijs je zo een waarde aan een sleutel toe:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Uitvoer: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

Toegang tot een waarde via de sleutel is eenvoudig:

```go
fmt.Println("De hexcode voor rood is:", colors["red"])
// Uitvoer: De hexcode voor rood is: #FF0000
```

Om een element te verwijderen, gebruik je de `delete` functie:

```go
delete(colors, "red")
fmt.Println(colors)
// Uitvoer: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

Het itereren over een map gebeurt met een for-lus:

```go
for color, hex := range colors {
    fmt.Printf("Sleutel: %s Waarde: %s\n", color, hex)
}
```

Onthoud, maps in Go zijn ongeordend. De volgorde van iteratie is niet gegarandeerd.

## Diepgaand

In Go zijn maps geïmplementeerd als hashtabellen. Elk item in de map bestaat uit twee onderdelen: een sleutel en een waarde. De sleutel wordt gehasht om het item op te slaan, wat constante tijdoperaties mogelijk maakt voor een kleine set aan data en gemiddelde tijdscomplexiteit van O(1) met goede hashing, wat kan verslechteren tot O(n) in het slechtste geval met veel hashbotsingen.

Een belangrijke opmerking voor nieuwe Go-programmeurs is dat map-typen referentietypen zijn. Dit betekent dat wanneer je een map aan een functie doorgeeft, eventuele wijzigingen die binnen die functie aan de map worden gemaakt, zichtbaar zijn voor de aanroeper. Dit is anders dan bijvoorbeeld het doorgeven van een struct aan een functie, waarbij de struct gekopieerd wordt, tenzij deze door een pointer wordt doorgegeven.

Hoewel maps ongelooflijk veelzijdig en efficiënt zijn voor de meeste gebruiksscenario's met associatieve arrays, kan het in prestatie-kritieke toepassingen voordelig zijn om gegevensstructuren met meer voorspelbare prestatiekenmerken te gebruiken, vooral als sleutelverdelingen frequent botsingen kunnen veroorzaken.

Een ander alternatief om te overwegen is de `sync.Map`, beschikbaar sinds Go 1.9, ontworpen voor gebruiksscenario's waarbij sleutels slechts eenmaal worden geschreven maar vele malen worden gelezen, wat efficiëntieverbeteringen in deze scenario's biedt. Echter, voor conventionele Go-toepassingen is het reguliere gebruik van maps idiomatisch en vaak de aanbevolen aanpak vanwege de eenvoud en directe ondersteuning in de taal.
