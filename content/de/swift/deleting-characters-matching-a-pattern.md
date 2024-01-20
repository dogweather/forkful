---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Swift Tutorials: Zeichen anhand eines Musters löschen

## Was & Warum?

Die Löschung von Zeichen, die einem Muster entsprechen, ist ein häufig genutztes Konzept in der Programmierung. Es hilft, unerwünschte Zeichen zu entfernen und sauberen, analysierbaren Code zu erstellen.

## Wie geht's:

In Swift können wir die `replacingOccurrences` Funktion verwenden, um Zeichen zu löschen, die einem bestimmten Muster entsprechen. Hier ist ein einfaches Beispiel:

```Swift
let text = "Hallo, ich lerne Swift!"
let cleanText = text.replacingOccurrences(of: ",", with: "")
print(cleanText)
```
Ausgabe:

```Swift
Hallo ich lerne Swift!
```
In diesem Fall löschen wir einfach alle Kommas aus dem Text.

## Tief tauchen: 

1. Historischer Kontext: 

Historisch gesehen, umfasst das Löschen von Zeichen, die einem Muster entsprechen, eine Vielzahl von Techniken, von regulären Ausdrücken bis hin zu spezifischen Algorithmen. Heutzutage bieten die meisten Sprachen eingebaute Funktionen dafür, einschließlich Swift.

2. Alternativen: 

Eine Alternative könnte darin bestehen, durch die Zeichen des Strings zu iterieren und alle Zeichen, die dem Muster entsprechen, zu überspringen. Aber `replacingOccurrences` ist effizienter und einfacher zu verwenden.

3. Implementierungsdetails: 

Bei `replacingOccurrences` handelt es sich um eine hochstabile Methode aus der Swift-Standardbibliothek. Sie verwendet einen Algorithmus, der die Zeichenkette durchgeht und jedes Auftreten des Musters durch eine andere Zeichenkette ersetzt. Wenn die Ersatzzeichenkette leer ist, erreichen wir das Löschen des Musters.

## Siehe auch:


2. Mehr zu regulären Ausdrücken: [Regular Expressions in Swift](https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started)

Lassen Sie uns auf diesem Weg bleiben, und Sie werden bald ein Swift-Profi! Happy Coding!