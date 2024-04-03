---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:53.493137-07:00
description: "Associatieve arrays, bekend als woordenboeken in Swift, laten je gegevens\
  \ opslaan en beheren als sleutel-waarde paren. Programmeurs gebruiken ze om\u2026"
lastmod: '2024-03-13T22:44:51.149314-06:00'
model: gpt-4-0125-preview
summary: Associatieve arrays, bekend als woordenboeken in Swift, laten je gegevens
  opslaan en beheren als sleutel-waarde paren.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe te:
Swift maakt werken met associatieve arrays eenvoudig. Hier is hoe je items kunt declareren, toevoegen, verwijderen en toegang krijgen in een Swift-woordenboek:

```Swift
// Een woordenboek declareren
var fruitKleuren: [String: String] = ["Appel": "Rood", "Banaan": "Geel"]

// Een nieuw item toevoegen
fruitKleuren["Druif"] = "Paars"

// Toegang krijgen tot een waarde met behulp van de sleutel
if let appelKleur = fruitKleuren["Appel"] {
    print("Appel is \(appelKleur).")  // Uitvoer: Appel is Rood.
} else {
    print("Kleur niet gevonden.")
}

// Een item verwijderen
fruitKleuren["Banaan"] = nil  // Dit zal "Banaan" uit het woordenboek verwijderen

// Itereren over items
for (fruit, kleur) in fruitKleuren {
    print("\(fruit) is \(kleur).")
    // Uitvoer:
    // Appel is Rood.
    // Druif is Paars.
}
```

Woordenboeken zijn ongelooflijk veelzijdig, waardoor je gegevens dynamisch kunt manipuleren en toegang krijgen. Hun ongeordende aard heeft geen impact op de snelheid van gegevensopvraging, wat een aanzienlijk voordeel is bij het omgaan met grote datasets.

## Diepgaand
Swift's implementatie van woordenboeken als een associatieve array komt voort uit hun krachtige vermogen om unieke sleutels aan waarden te koppelen. Historisch gezien hebben programmeertalen dit concept onder verschillende namen geïmplementeerd, zoals hash-tabellen of mappen, verwijzend naar hun functionaliteit om een "kaart" tussen sleutels en waarden te creëren.

In Swift zijn woordenboeken geoptimaliseerd voor prestaties, waarbij gebruik wordt gemaakt van hashbare sleutels voor efficiënte gegevensopvraging. Dit betekent dat het `Key` type in een `[Key: Value]` woordenboek moet voldoen aan het `Hashable` protocol, wat het geval is voor de meeste Swift standaardtypen zoals `Int`, `String` en `Double`.

Een punt van overweging is dat hoewel woordenboeken uitstekend zijn voor het associëren van paren van gegevens, ze geen volgorde hebben. Als je de volgorde van elementen moet behouden, zou je alternatieven kunnen verkennen zoals `Array` voor een reeks geordende elementen of aangepaste gegevensstructuren die de kenmerken van zowel arrays als woordenboeken combineren.

Het is ook opmerkelijk dat Swift continu evolueert, evenals de behandeling en optimalisaties van woordenboeken. Daarom is het cruciaal om up-to-date te blijven met de laatste Swift-documentatie om het meeste uit woordenboeken te halen, ervoor zorgend dat je de meest efficiënte en actuele praktijken gebruikt.
