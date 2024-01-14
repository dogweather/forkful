---
title:                "Swift: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum Sie eine Zeichenkette in Kleinbuchstaben umwandeln möchten. Zum Beispiel können Sie so sicherstellen, dass alle Benutzereingaben einheitlich verarbeitet werden, unabhängig davon, ob der Benutzer Groß- oder Kleinbuchstaben verwendet.

## Wie geht es

Es gibt verschiedene Möglichkeiten, eine Zeichenkette in Kleinbuchstaben umzuwandeln. Eine einfache Methode ist die Verwendung der ``String``-Methode ``lowercased()``. Hier ist ein Beispielcode:

```Swift
let name = "John Doe"
let lowercaseName = name.lowercased()
print(lowercaseName)
```

Die Ausgabe wird sein:

```
john doe
```

Wie Sie sehen, wird die ursprüngliche Zeichenkette, ``John Doe``, in eine neue Zeichenkette umgewandelt, die ausschließlich Kleinbuchstaben enthält.

Wenn Sie jedoch nur bestimmte Teile einer Zeichenkette in Kleinbuchstaben konvertieren möchten, können Sie die ``String``-Methode ``capitalized()`` in Kombination mit der Methode ``capitalized(with:)`` verwenden. Hier ist ein Beispielcode:

```Swift
let sentence = "Ich liebe es, Swift zu programmieren"
let lowercaseSentence = sentence.lowercased()
let sentenceWithCapitalizedFirstLetter = lowercaseSentence.capitalized(with: Locale.current)
print(sentenceWithCapitalizedFirstLetter)
```

Die Ausgabe wird sein:

```
Ich liebe es, swift zu programmieren
```

In diesem Beispiel wird die gesamte Zeichenkette in Kleinbuchstaben umgewandelt und dann wird die erste Buchstaben jeder Wort des Satzes großgeschrieben.

## Tiefere Einblicke

Wenn Sie tiefer in die Umwandlung von Zeichenketten in Kleinbuchstaben eintauchen möchten, gibt es einige Dinge zu beachten. Zum Beispiel gibt es Unterschiede in der Art und Weise, wie verschiedene Sprachen bestimmte Buchstaben in Groß- und Kleinbuchstaben umwandeln. Deshalb kann es hilfreich sein, die Methode ``lowercased(with:)`` zu verwenden und eine bestimmte Region anzugeben, um die Umwandlung korrekt durchzuführen.

Eine weitere Sache, die es zu beachten gilt, ist die Berücksichtigung von Sonderzeichen und Akzenten in Zeichenketten während der Umwandlung. Mit Swift gibt es jedoch einfache Möglichkeiten, diese zu erkennen und die Umwandlung entsprechend anzupassen.

Insgesamt gibt es viele Aspekte, die bei der Umwandlung von Zeichenketten in Kleinbuchstaben zu beachten sind. Es ist daher wichtig, sich damit vertraut zu machen und zu verstehen, wie sie in Ihrem spezifischen Anwendungsfall korrekt eingesetzt werden können.

## Siehe auch

- [Dokumentation von Apple über die ``String``-Methode ``lowercased()``](https://developer.apple.com/documentation/foundation/string/2893955-lowercased)
- [Dokumentation von Apple über die ``String``-Methode ``capitalized(with:)``](https://developer.apple.com/documentation/foundation/string/1654413-capitalized)
- [Tutorial zur Textmanipulation in Swift](https://www.raywenderlich.com/5485-text-manipulation-tutorial-for-ios-how-to-translate-strings-formatting-for-localization)