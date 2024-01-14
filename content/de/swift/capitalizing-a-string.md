---
title:    "Swift: Ein String großschreiben"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings kann in Swift sehr nützlich sein, um die Darstellung von Text in einer App zu verbessern oder um spezifische Anforderungen in einer Anwendung zu erfüllen.

## Wie

Das Kapitalisieren von Strings ist in Swift sehr einfach. Man kann einfach die `capitalized` Methode auf einen String anwenden, um den ersten Buchstaben jedes Worts im String zu groß zu schreiben.

```Swift
let string = "hello world"
print(string.capitalized)
```

Die Ausgabe wäre dann `Hello World`.

Man kann auch die `uppercased` Methode nutzen, um den gesamten String in Großbuchstaben zu schreiben.

```Swift
let string = "hello world"
print(string.uppercased)
```

Die Ausgabe wäre dann `HELLO WORLD`.

Um nur den ersten Buchstaben des Strings zu groß zu schreiben, kann die `prefix` Methode verwendet werden.

```Swift
let string = "hello world"
print(string.prefix(1).uppercased() + string.dropFirst())
```

Die Ausgabe wäre ebenfalls `Hello world`.

## Deep Dive

Bei der Verwendung von `capitalized` muss beachtet werden, dass nur der erste Buchstabe jedes Worts im String groß geschrieben wird. Das bedeutet, dass Namen oder Abkürzungen, die bereits Großbuchstaben enthalten, nicht verändert werden.

Um dies zu vermeiden, kann die `localizedCapitalized` Methode verwendet werden, die berücksichtigt, ob das erste Zeichen möglicherweise bereits ein Großbuchstabe ist.

```Swift
let string = "STring with ABBREVIATION"
print(string.localizedCapitalized)
```

Die Ausgabe wäre dann `String with Abbreviation`.

## Siehe auch
- [Apple Dokumentation zur capitalizing Methode](https://developer.apple.com/documentation/foundation/nsstring/1411949-capitalized)
- [ZipperLogic: Capitalize First Letter of a String in Swift](https://zipperlogic.com/swift-capitalize-first-letter-of-a-string/)
- [Hacking with Swift: How to capitalize the first letter of a string](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)