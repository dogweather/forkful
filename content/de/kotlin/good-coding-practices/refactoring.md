---
date: 2024-01-26 01:42:45.059682-07:00
description: "Refactoring ist der Prozess, bestehenden Code zu \xFCberarbeiten, um\
  \ seine Struktur, Lesbarkeit und Leistung zu verbessern, ohne sein externes Verhalten\
  \ zu\u2026"
lastmod: '2024-03-11T00:14:27.748192-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess, bestehenden Code zu \xFCberarbeiten, um seine\
  \ Struktur, Lesbarkeit und Leistung zu verbessern, ohne sein externes Verhalten\
  \ zu\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess, bestehenden Code zu überarbeiten, um seine Struktur, Lesbarkeit und Leistung zu verbessern, ohne sein externes Verhalten zu verändern. Programmierer führen ein Refactoring durch, um Code wartbarer zu machen, das Hinzufügen neuer Funktionen zu vereinfachen und Fehler leichter zu finden und zu beheben.

## Wie geht das:
Hier ist ein Kotlin-Schnipsel, der einen häufigen Code-Geruch und seine überarbeitete Version zeigt. Wir beginnen mit einem Code-Stück, das zu viel macht:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("Order ID: ${order.id}")
        // Berechnung der Bestellsumme
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Rabatt anwenden
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Weitere Verarbeitung...
    }
}
```

Überarbeitet für bessere Lesbarkeit und Trennung der Zuständigkeiten:

```kotlin
fun printOrderSummary(order: Order) {
    print("Order ID: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Keine Beispiel-Ausgabe hier, da wir die Funktionalität nicht verändert haben, aber die Lesbarkeit und Wartbarkeit des Codes wurde enorm verbessert!

## Tiefergehende Betrachtung
Refactoring als Konzept gibt es, seit es die Programmierung gibt, aber es hat sich vor allem in den 1990er Jahren als Disziplin durchgesetzt, besonders nachdem Martin Fowler "Refactoring: Improving the Design of Existing Code" im Jahr 1999 veröffentlicht hat. Dieses Buch gab der Praxis einen Namen und definierte eine organisierte Methode für ihre Anwendung, einschließlich eines Katalogs von Refactoring-Techniken.

Refactoring im Vergleich zu Alternativen: Man könnte den Code von Grund auf neu schreiben (riskant und zeitaufwendig) oder einfach additive Änderungen vornehmen (führt zu Softwareaufblähung und potenziellem Technologie-Schulden). Refactoring findet den Sweet Spot – es modernisiert und räumt auf, während es das Risiko gering hält.

Umsetzungstechnisch ist es essenziell, vor dem Beginn des Refactorings einen robusten Satz von Tests zu haben, um sicherzustellen, dass man das Verhalten des Programms nicht versehentlich ändert. Viele moderne IDEs (einschließlich IntelliJ für Kotlin) verfügen über automatisierte Refactoring-Tools zum Umbenennen von Variablen, Extrahieren von Methoden und mehr, was den Prozess beschleunigen und Fehler reduzieren kann.

## Siehe auch
- "Refactoring: Improving the Design of Existing Code" von Martin Fowler (für die grundlegende Arbeit zu diesem Thema)
- Kotlin-Dokumentation zu Kodierungskonventionen: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (um den 'Kotlin-Weg' für sauberen Code zu verstehen)
- JetBrains-Unterstützung für Refactoring in IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (für die praktische Nutzung von Refactoring-Tools)
- Googles Leitfaden für Refactoring in großem Maßstab: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (für Einblicke in größere Refactoring-Herausforderungen)
