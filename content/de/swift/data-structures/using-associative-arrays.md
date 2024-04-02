---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:56.290726-07:00
description: "Assoziative Arrays, in Swift als W\xF6rterb\xFCcher (dictionaries) bekannt,\
  \ erm\xF6glichen es Ihnen, Daten als Schl\xFCssel-Wert-Paare zu speichern und zu\
  \ verwalten.\u2026"
lastmod: '2024-03-13T22:44:54.218710-06:00'
model: gpt-4-0125-preview
summary: "Assoziative Arrays, in Swift als W\xF6rterb\xFCcher (dictionaries) bekannt,\
  \ erm\xF6glichen es Ihnen, Daten als Schl\xFCssel-Wert-Paare zu speichern und zu\
  \ verwalten.\u2026"
title: Verwendung von assoziativen Arrays
weight: 15
---

## Was & Warum?

Assoziative Arrays, in Swift als Wörterbücher (dictionaries) bekannt, ermöglichen es Ihnen, Daten als Schlüssel-Wert-Paare zu speichern und zu verwalten. Programmierer verwenden sie, um Daten effizient zu organisieren, was den Zugriff und die Manipulation von Werten basierend auf ihren einzigartigen Schlüsseln erleichtert.

## Wie man:

Swift macht die Arbeit mit assoziativen Arrays unkompliziert. Hier ist, wie Sie ein Swift-Wörterbuch deklarieren, hinzufügen, entfernen und Elemente darin zugreifen können:

```Swift
// Ein Wörterbuch deklarieren
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Ein neues Element hinzufügen
fruitColors["Grape"] = "Purple"

// Auf einen Wert zugreifen, indem man seinen Schlüssel verwendet
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // Ausgabe: Apple is Red.
} else {
    print("Farbe nicht gefunden.")
}

// Ein Element entfernen
fruitColors["Banana"] = nil  // Dies wird "Banana" aus dem Wörterbuch entfernen

// Über Elemente iterieren
for (fruit, color) in fruitColors {
    print("\(fruit) ist \(color).")
    // Ausgabe:
    // Apple ist Red.
    // Grape ist Purple.
}
```

Wörterbücher sind unglaublich vielseitig, da sie es Ihnen ermöglichen, dynamisch auf Daten zuzugreifen und diese zu manipulieren. Ihre ungeordnete Natur hat keinen Einfluss auf die Geschwindigkeit des Datenabrufs, was ein bedeutender Vorteil beim Umgang mit großen Datenmengen ist.

## Vertiefung

Die Implementierung von Wörterbüchern als assoziative Arrays in Swift stammt aus ihrer leistungsfähigen Fähigkeit, eindeutige Schlüssel zu Werten zuzuordnen. Historisch gesehen haben Programmiersprachen dieses Konzept unter verschiedenen Namen wie Hash-Tabellen oder Maps implementiert, was auf ihre Funktionalität anspielt, eine "Karte" zwischen Schlüsseln und Werten zu erstellen.

In Swift sind Wörterbücher für die Leistung optimiert, indem sie hashbare Schlüssel für effizienten Datenabruf nutzen. Dies bedeutet, dass der `Key` Typ in einem `[Key: Value]` Wörterbuch dem `Hashable` Protokoll entsprechen muss, was bei den meisten Standardtypen von Swift wie `Int`, `String` und `Double` der Fall ist.

Etwas zu bedenken ist, dass Wörterbücher zwar hervorragend für die Zuordnung von Datenpaaren sind, ihnen jedoch die Ordnung fehlt. Wenn Sie die Reihenfolge der Elemente beibehalten müssen, könnten Sie Alternativen wie `Array` für eine Sequenz von geordneten Elementen oder benutzerdefinierte Datenstrukturen, die die Merkmale von Arrays und Wörterbüchern kombinieren, erkunden.

Es ist auch bemerkenswert, dass Swift kontinuierlich weiterentwickelt wird, und so auch seine Handhabung und Optimierungen von Wörterbüchern. Daher ist es entscheidend, mit der neuesten Swift-Dokumentation auf dem Laufenden zu bleiben, um das Beste aus Wörterbüchern herauszuholen und sicherzustellen, dass Sie die effizientesten und aktuellsten Praktiken verwenden.
