---
title:                "Refactoring"
date:                  2024-01-26T01:18:04.732131-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/refactoring.md"
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess des Überarbeitens Ihres Codes, um ihn sauberer und wartbarer zu machen, ohne sein externes Verhalten zu ändern. Programmierer führen ein Refactoring durch, um die Lesbarkeit zu verbessern, die Komplexität zu reduzieren und den Code leichter an zukünftige Updates oder Feature-Ergänzungen anpassen zu können.

## Wie:
Nehmen wir an, Sie haben einen Abschnitt Ihres Codes, in dem Sie einige wiederholte Berechnungen oder Zeichenkettenmanipulationen über mehrere Funktionen hinweg durchführen. Das ist ein Hauptziel für Refactoring. Hier ist ein Vorher-Nachher-Beispiel mit Gleam, das einen starken Schwerpunkt auf Typsicherheit und Unveränderlichkeit legt:

```gleam
// Vor dem Refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(width: Int, height: Int) {
  let area = calculate_area(width, height)
  io.println("Die Fläche beträgt \(area)")
}

// Nach dem Refactoring
pub fn calculate_area(width: Int, height: Int) -> Int {
  width * height
}

pub fn print_area(area: Int) {
  io.println("Die Fläche beträgt \(area)")
}

// In einem anderen Teil Ihres Codes rufen Sie print_area so auf:
print_area(calculate_area(10, 20))
```

Beispieloutput:
```
Die Fläche beträgt 200
```

Durch das Refactoring haben wir `print_area` fokussierter auf das Drucken gemacht, während die Berechnung anderswo gehandhabt wird. Das macht den Code modularer und leichter wiederverwendbar oder testbar.

## Tiefergehend
Refactoring als Konzept gibt es so lange wie das Programmieren selbst – das Überarbeiten und Säubern von Code ist Teil der guten Haushaltung. Die moderne Formalisierung des Refactorings, zusammen mit vielen der heute verwendeten Techniken und Muster, lässt sich auf Martin Fowlers bahnbrechendes Buch "Refactoring: Improving the Design of Existing Code" zurückführen, das 1999 veröffentlicht wurde.

Im Gleam-Ökosystem hat das Refactoring spezifische Überlegungen. Eine der bedeutendsten ist die starke Typüberprüfung zur Kompilierzeit, welche helfen kann, Fehler frühzeitig zu erkennen, wenn Sie Dinge verschieben. Gleams Musterabgleich und Unveränderlichkeitsfunktionen können Sie auch dazu anleiten, klareren, prägnanteren Code zu schreiben – eines der Hauptziele des Refactorings.

Alternativen zum Refactoring könnten das Neuschreiben von Code von Grund auf oder das Patchen von Code mit schnellen Fixes umfassen. Refactoring ist jedoch in der Regel der sicherste und effizienteste Ansatz zur Verbesserung bestehender Codes, ohne neue Bugs einzuführen. Es beinhaltet schrittweise, gut unterstrichene, verhaltensbewahrende Transformationen.

## Siehe auch
- Martin Fowlers Buch "Refactoring": https://martinfowler.com/books/refactoring.html
- Die Gleam-Sprachwebsite, mit zusätzlicher Dokumentation und Beispielen: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" von Martin Fowler (für zugrundeliegende Prinzipien, die sprachübergreifend anwendbar sind): https://martinfowler.com/books/refactoring.html