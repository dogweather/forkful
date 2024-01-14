---
title:                "Swift: Das Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Das Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann eine sehr nützliche Funktion sein, um unerwünschte Daten aus einer Zeichenfolge zu entfernen. Zum Beispiel können HTML-Tags aus einer Webseite entfernt werden, um nur den reinen Text zu behalten.

# Wie man es macht

Die Swift-Programmiersprache bietet verschiedene Möglichkeiten, um Zeichenfolgen zu durchsuchen und bestimmte Zeichen oder Muster zu löschen.

Eine einfache Möglichkeit ist die Verwendung des `replacingOccurrences`-Methode, die Teil der `String`-Klasse ist. Diese Methode nimmt zwei Parameter an: den zu suchenden String und den String, der anstelle davon gesetzt werden soll. Nehmen wir an, wir möchten alle Vokale aus einer Zeichenfolge entfernen. Der folgende Code zeigt, wie wir diese Methode verwenden können:

```Swift
let string = "Guten Morgen"
let modifiedString = string.replacingOccurrences(of: "[AEIOUaeiou]", with: "", options: .regularExpression)
print(modifiedString) // Gtn Mrngn
```

Wie wir sehen können, haben wir die Vokale durch einen leeren String ersetzt, was sie praktisch gelöscht hat.

Eine weitere Möglichkeit ist die Verwendung von Regular Expressions. Diese bieten eine leistungsstarke Möglichkeit, Muster in einer Zeichenfolge zu identifizieren und zu manipulieren. Hier ist ein Beispiel, wie wir Regular Expressions verwenden können, um Telefonnummern aus einer Zeichenfolge zu entfernen:

```Swift
let string = "Meine Telefonnummer ist 012-345-6789. Bitte rufen Sie mich an."
let regex = try! NSRegularExpression(pattern: "[0-9]{3}-[0-9]{3}-[0-9]{4}", options: .caseInsensitive)
let modifiedString = regex.stringByReplacingMatches(in: string, options: [], range: NSRange(0..<string.utf16.count), withTemplate: "")
print(modifiedString) // Meine Telefonnummer ist . Bitte rufen Sie mich an.
```

Hier haben wir ein Muster definiert, das einer typischen Telefonnummer entspricht (in diesem Fall "012-345-6789") und es durch einen leeren String ersetzt.

# Tiefer eintauchen

Das Löschen von Zeichen oder Mustern aus einer Zeichenfolge ist nur eine Möglichkeit, sie zu bearbeiten. Es gibt viele weitere nützliche Funktionen, die Ihnen helfen können, bestimmte Daten aus einer Zeichenfolge zu extrahieren oder zu manipulieren.

Sie können auch mehr über Regular Expressions lernen, um komplexere Muster zu identifizieren und zu manipulieren. Die Verwendung von Libraries wie `NSRegularExpression` und `CharacterSet` kann auch sehr hilfreich sein.

# Siehe auch

- [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [NSRegularExpression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [CharacterSet Documentation](https://developer.apple.com/documentation/foundation/characterset)