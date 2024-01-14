---
title:    "Swift: Zusammenfügen von Zeichenketten"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in der Swift Programmiersprache arbeitest, wirst du wahrscheinlich irgendwann auf die Notwendigkeit stoßen, mehrere Strings miteinander zu verbinden. Das kann zum Beispiel beim Erstellen von Benutzerinteraktionen oder beim Zusammenfügen von Benutzer- und Systemdaten nützlich sein. In diesem Blogbeitrag werde ich dir zeigen, wie du einfach und effektiv Strings in Swift zusammenfügen kannst.

## Wie geht das?

Die einfachste Methode zum Zusammenfügen von Strings in Swift ist die Verwendung des `+` Operators. Schauen wir uns ein Beispiel an:

```Swift 
let name = "Sophia"
let profession = "Programmiererin"
let phrase = name + " ist eine erfolgreiche " + profession.
```

Das Ergebnis der Variable `phrase` ist "Sophia ist eine erfolgreiche Programmiererin". Wie du sehen kannst, werden die einzelnen Strings einfach hintereinander angehängt.

Eine weitere Möglichkeit ist die Verwendung von String Interpolation. Dabei werden Variablen direkt in einen String eingebettet, indem sie von einem BACKSLASH gefolgt werden. Schauen wir uns ein Beispiel an:

```Swift 
let age = 28
let info = "Sophia ist \(age) Jahre alt."
```

Das Ergebnis der Variable `info` ist "Sophia ist 28 Jahre alt." Hier wird die Variable `age` direkt in den String eingefügt, ohne dass der `+` Operator verwendet werden muss.

## Deep Dive

Wenn du noch tiefer in das Thema des Zusammenfügens von Strings in Swift eintauchen möchtest, gibt es noch einige weitere interessante Möglichkeiten. Zum Beispiel kannst du mit der `+=` Operator auch Strings aneinanderhängen, anstatt sie neu zu initialisieren.

Außerdem bietet Swift eine praktische Funktion namens `joined(separator:)`, mit der du ein Array von Strings mit einem bestimmten Trennzeichen verbinden kannst. Schauen wir uns ein Beispiel an:

```Swift 
let hobbies = ["Klettern", "Reisen", "Programmieren"]
let hobbyList = hobbies.joined(separator: ", ")
```

Das Ergebnis der Variable `hobbyList` ist "Klettern, Reisen, Programmieren". Hier wurde jede der Hobbys im Array durch ein Komma und ein Leerzeichen getrennt.

## Siehe auch

- [Offizielle Swift Dokumentation zum Zusammenfügen von Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID285)
- [Swift-Tutorial: Einführung in Strings](https://www.raywenderlich.com/11520-swift-tutorial-a-quick-start)
- [Interpolation in Swift](https://www.swiftbysundell.com/articles/string-interpolation-in-swift/)