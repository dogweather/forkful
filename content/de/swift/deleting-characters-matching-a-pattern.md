---
title:                "Swift: Übereinstimmende Zeichen löschen"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in verschiedenen Situationen nützlich sein. Zum Beispiel könnten Sie in einem String alle Zahlen entfernen, um nur die Buchstaben zu behalten. Oder Sie möchten bestimmte Sonderzeichen aus einem Text entfernen, um ihn für eine bestimmte Verarbeitung vorzubereiten. Die Löschung von Zeichen entsprechend einem Muster kann auch dazu beitragen, eine effizientere Suche in einem String oder einer Datenstruktur durchzuführen.

## Wie

Um Zeichen in Swift zu löschen, die einem bestimmten Muster entsprechen, gibt es verschiedene Ansätze. Hier sind zwei Beispiele:

### Beispiel 1: Löschen aller Zahlen aus einem String

```Swift
let string = "Ich bin 25 Jahre alt"
let numbersSet = CharacterSet.decimalDigits // Definiert ein Set von Zahlen
let result = string.components(separatedBy: numbersSet).joined() // Entfernt alle Zahlen aus dem String und fügt die verbleibenden Teile wieder zusammen
print(result) // Ausgabe: Ich bin Jahre alt
```

### Beispiel 2: Löschen von Sonderzeichen aus einem String

```Swift
let string = "Willkommen in unserem Blog! :)"
let punctuationSet = CharacterSet.punctuationCharacters // Definiert ein Set von Sonderzeichen
let result = string.trimmingCharacters(in: punctuationSet) // Entfernt alle Sonderzeichen am Anfang und am Ende des Strings
print(result) // Ausgabe: Willkommen in unserem Blog
```

## Deep Dive

Für eine detailliertere Erklärung darüber, wie das Löschen von Zeichen entsprechend einem Muster in Swift funktioniert, kann die offizielle Dokumentation von Apple hilfreich sein. Dort werden verschiedene Methoden und Klassen zur Verfügung gestellt, die bei der Verarbeitung und Manipulation von Strings helfen.

## Siehe auch

- [Offizielle Dokumentation von Apple zu Strings in Swift](https://developer.apple.com/documentation/foundation/string)
- [Tutorial zum Verarbeiten von Strings mit Swift](https://www.raywenderlich.com/165761/swift-tutorial-strings-in-swift-4)
- [Tutorial zur Verwendung von CharacterSets in Swift](https://www.hackingwithswift.com/articles/190/how-to-use-nscharacterset-to-split-up-a-string)