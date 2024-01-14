---
title:                "Swift: Ein String großschreiben"
simple_title:         "Ein String großschreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Großschreibung eines Strings ist eine der grundlegendsten Funktionen in der Swift-Programmierung. Es ermöglicht es, einen Text in einem einheitlichen Format darzustellen, was besonders wichtig ist, wenn wir mit Benutzereingaben oder Daten aus externen Quellen arbeiten. Aber warum sollten wir uns die Mühe machen, einen String zu kapitalisieren? Nun, es sorgt nicht nur für ein ästhetisch ansprechenderes Erscheinungsbild, sondern kann auch bei der Suche, Sortierung oder Vergleich von Strings hilfreich sein. In diesem Blogbeitrag erfährst du, wie du Strings in Swift ganz einfach kapitalisieren kannst.

## Wie geht's

Um einen String in Swift zu kapitalisieren, gibt es verschiedene Möglichkeiten, je nachdem wie deine Anforderungen sind.

### Die `capitalized` Methode

Die einfachste Methode ist die Verwendung der `capitalized` Methode. Diese Methode wandelt den ersten Buchstaben jedes Worts im String automatisch in einen Großbuchstaben um.

```Swift
let name = "max muster"
print(name.capitalized) // Ausgabe: Max Muster
```

Wie du sehen kannst, wird der Name "max muster" in "Max Muster" umgewandelt. Diese Methode ist nützlich, wenn du sicherstellen möchtest, dass dein String immer mit einem Großbuchstaben beginnt.

### Die `uppercased` Methode

Manchmal möchtest du vielleicht, dass der gesamte String in Großbuchstaben angezeigt wird. Dafür kannst du die `uppercased` Methode verwenden.

```Swift
let text = "Hallo, wie geht es dir?"
print(text.uppercased()) // Ausgabe: HALLO, WIE GEHT ES DIR?
```

Mit dieser Methode kannst du deinen String komplett in Großbuchstaben ändern.

### Die `lowercased` Methode

Ähnlich wie bei `uppercased` kannst du mithilfe der `lowercased` Methode deinen String in Kleinbuchstaben umwandeln.

```Swift
let phrase = "ALLES GROẞGESCHRIEBEN"
print(phrase.lowercased()) // Ausgabe: alles großgeschrieben
```

Diese Methode ist besonders nützlich, wenn du sicherstellen möchtest, dass alle Benutzereingaben in Kleinbuchstaben gespeichert werden, um mögliche Inkonsistenzen zu vermeiden.

### Die `capitalized(with:)` Funktion

Mithilfe der `capitalized(with:)` Funktion kannst du auch bestimmte Anforderungen an die Großschreibung stellen. Du kannst zum Beispiel festlegen, dass der erste Buchstabe in jedem Satz großgeschrieben wird.

```Swift
let text = "ich bin stolz ein Programmierer zu sein. und auf Swift besonders."
print(text.capitalized(with: .individualWords)) // Ausgabe: Ich Bin Stolz Ein Programmierer Zu Sein. Und Auf Swift Besonders.
```

In diesem Beispiel wird der erste Buchstabe jedes Satzes großgeschrieben, während alle anderen Buchstaben klein bleiben.

## Deep Dive

Hinter den Kulissen überprüft die Großschreibung in Swift jedes Zeichen in einem String und wendet die entsprechenden Änderungen an. Dies bedeutet, dass du auch Sonderzeichen und Buchstaben aus anderen Sprachen mit Großbuchstaben umwandeln kannst.

Wichtig zu beachten ist auch, dass die Großschreibung in Swift nur auf Strings funktioniert. Wenn du also versuchst, einen Integer oder ein Array zu kapitalisieren, wird es zu einem Fehler führen.

## Siehe auch

- [Swift Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Capitalizing Strings in Swift](https://www.hackingwithswift.com/articles/167/how-to-capitalise-the-first-letter-of-a-string)
- [String Capitalization in Swift](https://www.avanderlee.com/swift/string-capitalization-swift/)