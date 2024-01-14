---
title:                "Swift: String in Kleinbuchstaben umwandeln"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren einer Zeichenfolge in Kleinbuchstaben ist eine nützliche Fähigkeit, die jeder Swift-Programmierer beherrschen sollte. Dies ermöglicht es, Texte zu formatieren und zu vergleichen und macht den Code insgesamt sauberer und lesbarer.

## Wie man es macht

Um eine Zeichenfolge in Kleinbuchstaben umzuwandeln, können wir die `lowercased()` Methode verwenden. Dies funktioniert sowohl für einzelne Zeichenfolgen als auch für String-Arrays.

```Swift

let string = "DIESEN TEXT IN KLEINBUCHSTABEN KONVERTIEREN"
print(string.lowercased())
// output: diesen text in kleinbuchstaben konvertieren

let stringArray = ["Apple", "BANANA", "ORANGE"]
for fruit in stringArray {
    print(fruit.lowercased())
}
// output: apple, banana, orange
```

## Tiefer Blick

Wenn wir uns den Code genauer ansehen, werden wir feststellen, dass die `lowercased()` Methode eine *neue* Zeichenfolge zurückgibt, anstatt die vorhandene zu ändern. Das bedeutet, dass wir das Ergebnis in einer neuen Variablen speichern müssen, wenn wir die konvertierte Zeichenfolge verwenden möchten.

Eine andere Möglichkeit, eine Zeichenfolge in Kleinbuchstaben zu konvertieren, ist die Verwendung der `localizedLowercase` Eigenschaft. Diese Methode berücksichtigt die Sprach- und Regionseinstellungen des Benutzers und konvertiert die Zeichenfolge entsprechend.

## Siehe auch

Hier sind noch einige nützliche Ressourcen, um mehr über die Arbeit mit Zeichenfolgen in Swift zu erfahren:

- [Swift Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple Developer Documentation: String](https://developer.apple.com/documentation/foundation/string)
- [Hacking with Swift: How to use string interpolation in Swift](https://www.hackingwithswift.com/articles/141/how-to-use-string-interpolation-in-swift)