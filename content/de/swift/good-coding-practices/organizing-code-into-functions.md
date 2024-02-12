---
title:                "Code in Funktionen organisieren"
aliases: - /de/swift/organizing-code-into-functions.md
date:                  2024-01-26T01:11:49.838528-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren, bedeutet, Aufgaben in wiederverwendbare Blöcke zu zerlegen. Es macht den Code sauberer, weniger fehleranfällig und einfacher zu debuggen oder zu refaktorisieren.

## Wie geht das:
Stellen Sie sich eine Aufgabe vor: Berechnen Sie den Durchschnitt eines Arrays. Ohne Funktionen würden Sie alles im Hauptteil belassen. Mit Funktionen würden Sie es so machen:

```swift
func berechneDurchschnitt(von zahlen: [Double]) -> Double {
    let summe = zahlen.reduce(0, +)
    return zahlen.isEmpty ? 0 : summe / Double(zahlen.count)
}

// Verwendung
let bewertungen = [92.5, 88.75, 99.0, 70.5]
let durchschnittsbewertung = berechneDurchschnitt(von: bewertungen)
print("Durchschnittsbewertung ist \(durchschnittsbewertung)")
```

Die Beispiel-Ausgabe wäre:
```
Durchschnittsbewertung ist 87.6875
```

## Tiefergehend
Historisch gesehen sind Funktionen mit der zunehmenden Komplexität der Programmierung ein Grundstein für das Management von Komplexität geworden. Alternativen umfassen Inline-Coding und das Kopieren und Einfügen von Code (Spaghetti-Code) – dies wird mittlerweile weitgehend als schlechte Praxis angesehen. In Swift sind Funktionen Bürger erster Klasse; sie können Variablen zugewiesen werden, als Argumente übergeben werden und von anderen Funktionen zurückgegeben werden, wodurch der Code modularer und flexibler wird.

Bei der Implementierung gilt: Gestalten Sie Ihre Funktionen so, dass sie eine Sache gut machen. Streben Sie nach Funktionen mit einem klaren Zweck und einem Namen, der dies widerspiegelt. Achten Sie auf die Anzahl der Parameter – zu viele bedeuten wahrscheinlich, dass Sie zu viel tun. Fehlerbehandlung? Berücksichtigen Sie werfende Funktionen und handhaben Sie Probleme mit Anmut. Denken Sie daran: Swift dreht sich alles um Lesbarkeit und einfache Wartung.

## Siehe auch
- [Swift Programmiersprachenhandbuch – Funktionen](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Swift Style Guide von Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Refactoring: Verbesserung des Designs von bestehendem Code von Martin Fowler](https://martinfowler.com/books/refactoring.html)
