---
title:                "Strings verketten"
html_title:           "Swift: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Manchmal muss man in der Programmierung Texte oder Variablen miteinander verbinden, sei es für die Ausgabe in einer Benutzeroberfläche oder zur Speicherung in einer Datei. In solchen Fällen ist das Zusammenfügen von Strings notwendig. In diesem Artikel werden wir uns ansehen, wie man dies in Swift macht.

## Wie geht das?

```Swift
let greeting = "Hallo"
let name = "Lisa"
let combined = greeting + name
print(combined)
```

Das obige Beispiel zeigt, wie man zwei Strings in Swift miteinander verbinden kann, indem man das Plus-Zeichen (+) verwendet. Dadurch wird der Wert der beiden Variablen zusammengefügt und in der Variablen "combined" gespeichert. Die Ausgabe wäre "HalloLisa".

Neben dem Plus-Zeichen gibt es auch die Option, die Funktion "String(format:)" zu verwenden. Diese ermöglicht es, Variablen oder Texte in einem bestimmten Format zu kombinieren.

```Swift
let number = 10
let result = String(format: "Die Zahl ist %d", number)
print(result)
```

Hier wird die Zahl 10 in den String eingefügt, um "Die Zahl ist 10" auszugeben.

## Tiefer tauchen

Es ist wichtig zu beachten, dass die Verwendung von "+" für die String-Konkatenation in Swift nicht die effizienteste Methode ist. Bei jedem Aufruf dieser Operation wird tatsächlich ein neuer String erstellt, anstatt einfach die vorhandenen Strings miteinander zu verbinden. Dies kann in komplexeren Anwendungen zu Leistungsproblemen führen. Um dieses Problem zu umgehen, kann man die Klasse "NSMutableString" verwenden, die eine effizientere Methode zur Manipulation von Strings bietet.

Eine weitere wichtige Sache zu beachten ist, dass das Zusammenfügen von Strings oft in Kombination mit der Formatierung von Texten verwendet wird. In solchen Fällen bietet Swift die Möglichkeit, String-Interpolation zu verwenden, bei der Variablen direkt in einen String eingefügt werden können, ohne die Verwendung von "+" oder "String(format:)".

```Swift
let num1 = 5
let num2 = 3
let result = "\(num1) plus \(num2) ergibt \(num1 + num2)"
print(result)
```

Dieses Beispiel zeigt, wie einfach es ist, Variablen in einen String einzufügen, um komplexe Ausdrücke zu erzeugen.

## Siehe auch

- [Offizielle Swift Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial: String concatenation in Swift](https://www.digitalocean.com/community/tutorials/how-to-concatenate-strings-in-swift)
- [Swift String Interpolation](https://www.hackingwithswift.com/example-code/language/what-is-string-interpolation-in-swift)