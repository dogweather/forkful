---
title:                "Substrings extrahieren"
html_title:           "Swift: Substrings extrahieren"
simple_title:         "Substrings extrahieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum

Es gibt viele Gründe, warum man substrings in Swift extrahieren möchte. Zum Beispiel kann es sein, dass man Daten analysieren oder bearbeiten muss, die in einem String gespeichert sind. Oder man möchte eine bestimmte Information aus einem längeren Text herausfiltern. Egal aus welchem Grund, die Möglichkeit, Substrings extrahieren zu können, ist eine nützliche Fähigkeit in der Swift Programmierung.

# Wie geht's

Um Substrings in Swift zu extrahieren, gibt es verschiedene Methoden, je nachdem was man erreichen möchte. Hier sind drei häufige Anwendungsbeispiele mit passendem Code und Output:

### Beispiel 1: Extrahieren von Zeichen aus einem String

Manchmal wollen wir nur ein oder mehrere Zeichen aus einem String extrahieren. Dafür können wir die Methode `prefix(_ maxLength: Int)` verwenden, die uns die ersten `maxLength` Zeichen des Strings zurückgibt.

```Swift
let text = "Swift ist eine tolle Programmiersprache"
let prefix = text.prefix(5)

print(prefix) // Output: Swift
```

### Beispiel 2: Extrahieren einer bestimmten Anzahl an Zeichen ab einer bestimmten Stelle

Oft ist es praktisch, wenn wir einen Teil eines Strings ab einer bestimmten Stelle herausfiltern können. Dafür nutzen wir die Methode `suffix(_ maxLength: Int)`, die uns die letzten `maxLength` Zeichen zurückgibt.

```Swift
let text = "Swift ist eine tolle Programmiersprache"
let suffix = text.suffix(16)

print(suffix) // Output: Programmiersprache
```

### Beispiel 3: Extrahieren eines bestimmten Teilstrings

Manchmal möchten wir auch einen bestimmten Teil eines Strings extrahieren, der an einer bestimmten Position beginnt und an einer anderen endet. Hierfür nutzen wir die Methode `subscript(_ bounds: Range<Int>)`, die uns den Teilstring zwischen den angegebenen Indizes zurückgibt.

```Swift
let text = "Swift ist eine tolle Programmiersprache"
let range = text.index(text.startIndex, offsetBy: 11)..<text.endIndex
let substring = text[range]

print(substring) // Output: tolle Programmiersprache
```

# Tiefer in die Materie

Wenn man sich tiefer mit dem Thema befassen möchte, gibt es noch weitere Methoden und Eigenschaften, die man beim Extrahieren von Substrings nutzen kann. Zum Beispiel gibt es die `range(of: String)` Methode, die uns die Position des ersten Vorkommens eines bestimmten Strings zurückgibt, oder die `components(separatedBy: String)` Methode, die uns den String in ein Array von Substrings aufteilt.

# Siehe auch

- [Offizielle Swift Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial über das Extrahieren von Substrings in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string-in-swift)
- [Weitere Beispiele und Erklärungen zum Thema Substrings](https://www.ralfebert.de/ios/swift-substring-range/)