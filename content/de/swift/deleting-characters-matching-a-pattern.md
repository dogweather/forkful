---
title:                "Entfernen von Zeichen, die einem Muster entsprechen"
html_title:           "Swift: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in Swift vielfältige Anwendungen haben. Zum Beispiel kann dies bei der Datenbereinigung oder der Verarbeitung von Benutzereingaben nützlich sein.

## Wie geht man vor?

Um in Swift Zeichen zu löschen, die einem bestimmten Muster entsprechen, gibt es verschiedene Ansätze. Zunächst muss das zu bearbeitende String-Objekt in ein Mutable String-Objekt umgewandelt werden, da Strings in Swift standardmäßig unveränderlich sind. Anschließend kann entweder die `filter`-Funktion oder die `replacingOccurrences`-Methode verwendet werden, um alle Zeichen, die dem Muster entsprechen, zu entfernen.

```Swift
var text = "Dies ist ein Beispieltext!"
// Umwandlung in ein Mutable String-Objekt
text = String(text)
// Verwendung der filter-Funktion
let filteredText = String(text.filter { !"aeiou".contains($0)})
print(filteredText) // Ds st n Bspltxtxt!

// Verwendung der replacingOccurrences-Methode
let replacedText = text.replacingOccurrences(of: "[ \\!]", with: "", options: .regularExpression)
print(replacedText) // DiesisteinBeispieltext
```

## Tiefere Einblicke

Wenn man genauer verstehen möchte, wie das Löschen von Zeichen, die einem bestimmten Muster entsprechen, funktioniert, ist es wichtig, die Konzepte von Mutable und Immutable in Swift zu verstehen. Mutable Objekte können verändert werden, während Immutable Objekte unveränderlich sind. In diesem Fall muss das String-Objekt in ein Mutable Objekt umgewandelt werden, damit die Funktion `filter` oder die Methode `replacingOccurrences` angewendet werden können.

Es ist auch wichtig zu beachten, dass die `filter`-Funktion eine neue Kopie des Strings erstellt, während die `replacingOccurrences`-Methode den vorhandenen String verändert. Je nach Anwendungsfall kann es sinnvoller sein, die eine oder die andere Methode zu verwenden.

## Siehe auch

- [Die offizielle Swift Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ray Wenderlich Artikel über Strings in Swift](https://www.raywenderlich.com/93964/strings-characters-and-unicode-in-swift)
- [Hacking with Swift Tutorial über String Manipulation](https://www.hackingwithswift.com/articles/141/4-ways-better-string-manipulation-in-swift)