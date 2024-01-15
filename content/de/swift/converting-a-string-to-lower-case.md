---
title:                "Eine Zeichenkette in Kleinbuchstaben umwandeln"
html_title:           "Swift: Eine Zeichenkette in Kleinbuchstaben umwandeln"
simple_title:         "Eine Zeichenkette in Kleinbuchstaben umwandeln"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal mit Strings in deinem Swift-Code gearbeitet hast, bist du vielleicht auf die Funktion `lowercased()` gestoßen. Aber warum ist es überhaupt wichtig, eine Zeichenkette in Kleinbuchstaben zu konvertieren?

Die Antwort ist, dass es viele Gründe dafür gibt, wie zum Beispiel für die String-Vergleich oder die Formatierung von Benutzereingaben. Es kann auch helfen, ungewollte Fehler in deinem Code zu vermeiden.

## Wie man eine Zeichenkette in Kleinbuchstaben konvertiert

Es gibt verschiedene Wege, um eine Zeichenkette in Kleinbuchstaben zu konvertieren. Hier sind zwei Beispiele mit dem Code und der dazugehörigen Ausgabe:

```Swift
let str1 = "Willkommen"
let lower1 = str1.lowercased()
print(lower1) // willkommen 
```

```Swift
var str2 = "Hallo, WELT"
str2 = str2.lowercased()
print(str2) // hallo, welt 
```

In beiden Fällen wird die Funktion `lowercased()` auf die Zeichenketten `str1` und `str2` angewendet. Anschließend wird die Ausgabe in Kleinbuchstaben angezeigt.

## Tiefere Einblicke

Um zu verstehen, wie die Konvertierung von Zeichenketten in Kleinbuchstaben funktioniert, müssen wir uns ein wenig mit Unicode beschäftigen.

Unicode ist ein Standard zur Darstellung von Schriftzeichen und Symbole in Computern. Jeder Buchstabe, jedes Zeichen oder jede Zahl hat in Unicode eine eindeutige Nummer, genannt Codepoint.

Die Funktion `lowercased()` verwendet diese Codepoints, um die Zeichenkette in Kleinbuchstaben umzuwandeln. Dabei werden auch Sonderzeichen, Emojis und andere Symbole berücksichtigt.

Um eine Zeichenkette in Großbuchstaben zu konvertieren, gibt es übrigens die Funktion `uppercased()`.

## Siehe auch

- [String lowercase() Method](https://developer.apple.com/documentation/swift/string/1786177-lowercased)
- [The Swift Programming Language - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)