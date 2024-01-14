---
title:    "Swift: Unterzeichenketten extrahieren"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Warum

Das Extrahieren von Teilstrings ist in der Swift-Programmierung extrem nützlich, um bestimmte Teile von Zeichenketten zu isolieren oder zu bearbeiten. Ob Sie eine Benutzereingabe validieren, Daten filtern oder einfach nur Text formatieren möchten, das Extrahieren von Teilstrings kann eine effiziente Lösung sein.

# Wie geht's

Die Grundlage für das Extrahieren von Teilstrings in Swift ist die `substring` Methode. Diese Methode erlaubt es uns, eine Teilzeichenkette aus einer ursprünglichen Zeichenkette zu extrahieren, indem wir den Start- und Endindex angeben. Sehen wir uns ein Beispiel an:

```swift
let fruit = "Apfel"
let startIndex = fruit.index(fruit.startIndex, offsetBy: 1) // startIndex wird den Index des zweiten Zeichens haben
let endIndex = fruit.index(fruit.endIndex, offsetBy: -1) // endIndex wird den Index des vorletzten Zeichens haben
let substring = fruit.substring(with: startIndex..<endIndex) // substring enthält jetzt "pfe"
print(substring) // Ausgabe: "pfe"
```

Diese Methode kann auch benutzt werden, um Teilstrings basierend auf einem bestimmten Zeichen zu extrahieren. Sehen wir uns ein weiteres Beispiel an:

```swift
let sentence = "Das ist ein Satz."
let index = sentence.firstIndex(of: " ") ?? sentence.endIndex // such nach dem ersten Leerzeichen
let firstWord = sentence.substring(to: index) // extrahiere den Text von Anfang bis zum Leerzeichen
print(firstWord) // Ausgabe: "Das"
```

Beachten Sie, dass wir bei der `substring(to:)` Methode den letzten Index nicht angegeben haben, was dazu führt, dass die Teilzeichenkette bis zum übergebenen Index extrahiert wird.

# Tiefgehender Einblick

Es gibt noch weitere Methoden, die für das Extrahieren von Teilstrings in Swift genutzt werden können, wie `range(of:)` oder `prefix`. Auch die Arbeit mit Unicode kann eine wichtige Rolle spielen, wenn es um das Extrahieren von Teilstrings geht.

Es ist auch erwähnenswert, dass es nicht empfohlen ist, Teilstrings zu ändern, da sie eine separate Zeichenkette sind und keine Referenzen auf die ursprüngliche Zeichenkette zurückhalten.

# Siehe auch

- [Swift Programmiersprache Offizielle Dokumentation - Strings und Zeichen](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Blog - Working with Strings in Swift](https://swift.org/blog/working-with-strings/)