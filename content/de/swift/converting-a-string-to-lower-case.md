---
title:    "Swift: Ein String in Kleinbuchstaben umwandeln"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie mit der Programmiersprache Swift arbeiten, werden Sie oft auf Strings als Datenstruktur stoßen. Strings sind einfach eine Sequenz von Zeichen, die verwendet werden, um Text darzustellen. Es gibt viele verschiedene Operationen, die Sie auf einem String durchführen können, z.B. die Konvertierung in Kleinbuchstaben. In diesem Blogbeitrag erfahren Sie, warum und wie Sie einen String in Swift in Kleinbuchstaben umwandeln können.

## Wie funktioniert es

Um einen String in Swift in Kleinbuchstaben zu konvertieren, können Sie die Methode `lowercased()` verwenden. Diese Methode gibt eine neue Version des Strings zurück, in der alle Zeichen in Kleinbuchstaben geschrieben sind. Wenn Sie den ursprünglichen String beibehalten möchten, können Sie den konvertierten String einer neuen Variablen zuweisen.

```Swift
let string = "PROGRAMMIEREN IST TOLL!"
let lowercaseString = string.lowercased()
print(lowercaseString)
// Output: programmieren ist toll!
```

Sie können auch die `map()`-Funktion verwenden, um jeden Buchstaben in einen Kleinbuchstaben zu konvertieren. Dies ist besonders nützlich, wenn Sie eine Liste von Strings haben und sie alle in Kleinbuchstaben konvertieren möchten.

```Swift
let strings = ["Schule", "Uni", "Studium"]
let lowercaseStrings = strings.map { $0.lowercased() }
print(lowercaseStrings)
// Output: ["schule", "uni", "studium"]
```

## Tiefergehende Informationen

Intern wandelt Swift jeden Buchstaben in einen Unicode-Wert um und vergleicht diesen dann mit einer Liste von Groß- und Kleinbuchstaben. Diese Konvertierung ist unabhängig von der Spracheinstellung des Geräts und folgt den offiziellen Unicode-Standards.

Es ist auch wichtig zu beachten, dass die `lowercased()`-Methode nur auf Strings funktioniert, die aus einer einzigen Zeile bestehen. Wenn Ihr String mehrere Zeilen oder Sonderzeichen enthält, sollten Sie die `localizedLowercase`-Eigenschaft verwenden, die spezielle Regeln für unterschiedliche Sprachen berücksichtigt.

## Siehe auch

- [String in Swift](https://www.swift-tutorial.de/swift-programmierung/swift-string/)
- [Unicode und Strings in Swift](https://swiftbysundell.com/tips/unicode-and-strings-in-swift/)
- [Offizielle Swift Dokumentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)