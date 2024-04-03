---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:34.244820-07:00
description: "Wie: Swifts `String`-Strukturen kommen mit ein paar integrierten Methoden,\
  \ um die Gro\xDF- und Kleinschreibung von Strings zu manipulieren. Hier sind einige\u2026"
lastmod: '2024-03-13T22:44:54.209456-06:00'
model: gpt-4-0125-preview
summary: "Swifts `String`-Strukturen kommen mit ein paar integrierten Methoden, um\
  \ die Gro\xDF- und Kleinschreibung von Strings zu manipulieren."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
Swifts `String`-Strukturen kommen mit ein paar integrierten Methoden, um die Groß- und Kleinschreibung von Strings zu manipulieren. Hier sind einige Ansätze, um Strings in Swift zu kapitalisieren, einschließlich der Verwendung von Standardmethoden und Drittanbieter-Bibliotheken, falls nötig.

### Verwendung eingebauter Methoden
Um den ersten Buchstaben eines Strings groß und den Rest klein zu schreiben:

```swift
let myString = "hallo, welt"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Ausgabe: "Hallo, welt"
```

Um den ersten Buchstaben jedes Wortes in einem Satz großzuschreiben, kann die `capitalized`-Eigenschaft verwendet werden:

```swift
let sentence = "hallo, welt"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Ausgabe: "Hallo, Welt"
```

### Verwendung einer Drittanbieter-Bibliothek
Obwohl die Standardbibliothek von Swift recht umfassend ist, könnten einige spezifische Großschreibungsformate komplexere Operationen erfordern oder können unter Verwendung von Drittanbieter-Bibliotheken vereinfacht werden. Eine der beliebten für die String-Manipulation ist SwiftRichString. (Hinweis: Stellen Sie immer sicher, dass Sie Drittanbieter-Bibliotheken über Swift Package Manager, CocoaPods oder Carthage hinzufügen und in Ihrer Datei importieren.)

Zuerst müssten Sie `SwiftRichString` zu Ihrem Projekt hinzufügen. Einmal installiert, können Sie es verwenden, um verschiedene String-Operationen durchzuführen, einschließlich spezifischer Großschreibungsbedürfnisse. Jedoch decken ab jetzt Swifts eingebaute Methoden die meisten Großschreibungsfälle ausreichend ab, ohne externe Bibliotheken nur für das Kapitalisieren von Strings zu benötigen.

Beziehen Sie sich immer auf die neueste Dokumentation der Bibliothek für alle Updates oder Änderungen in den Methoden.
