---
title:                "Die Länge eines Strings finden."
html_title:           "Swift: Die Länge eines Strings finden."
simple_title:         "Die Länge eines Strings finden."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Finden der Länge einer Zeichenkette ist ein grundlegender Schritt beim Programmieren. Es ermöglicht dir, die Anzahl der Zeichen in einer Zeichenkette zu bestimmen, was in vielen Szenarien wichtig ist. Zum Beispiel, um sicherzustellen, dass eine Eingabe innerhalb der erwarteten Länge liegt oder um bestimmte Formatierungen vorzunehmen.

## Wie geht's:
Um die Länge einer Zeichenkette zu finden, kannst du die Methode `count` verwenden, die in Swift integriert ist. Sie gibt die Anzahl der Zeichen in einer Zeichenkette zurück. Hier ist ein Beispiel:

```Swift
let string = "Hallo!"
print(string.count) // gibt 6 aus
```

## Tiefere Einblicke:
Das Konzept, die Länge einer Zeichenkette zu finden, ist nicht spezifisch für Swift, sondern wird in vielen Programmiersprachen verwendet. In älteren Sprachen wie C oder Java musste man eine Schleife verwenden und jedes Zeichen einzeln zählen, um die Länge einer Zeichenkette zu bestimmen. In Swift wird dies jedoch durch die `count` Methode vereinfacht.

Alternativ dazu gibt es auch die Möglichkeit, die Länge einer Zeichenkette mit der Eigenschaft `countElements` zu finden. Diese Methode zählt nicht die Anzahl der Zeichen, sondern die Anzahl der Unicode-Scalars in einer Zeichenkette. Dies kann in manchen Fällen zu unterschiedlichen Ergebnissen führen, daher ist es wichtig zu wissen, welche Methode für dein Szenario geeignet ist.

## Siehe auch:
- Offizielle Dokumentation zu `count` in Swift: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Stack Overflow: Unterschied zwischen `count` und `countElements` in Swift: https://stackoverflow.com/questions/26360057/count-vs-countelements-in-swift