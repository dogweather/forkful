---
title:    "Swift: Extrahieren von Teilzeichenketten"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings gehört zu den grundlegenden Aufgaben bei der Bearbeitung von Strings in Swift. Es ermöglicht uns, bestimmte Teilbereiche eines Textes zu isolieren und gezielt zu bearbeiten. In diesem Blogbeitrag werden wir uns genauer mit der Extrahierung von Teilstrings beschäftigen und verschiedene Möglichkeiten aufzeigen, wie wir dies in unserem Swift-Code umsetzen können.

## Wie geht das?

Um Teilstrings in Swift zu extrahieren, gibt es verschiedene Methoden und Techniken. Eine Möglichkeit ist die Verwendung der `substring()`-Funktion, die es uns ermöglicht, den Start- und Endindex des zu extrahierenden Teilstrings anzugeben. Schauen wir uns dazu ein Beispiel an:

```Swift
let str = "Hallo, Welt!"
let start = str.index(str.startIndex, offsetBy: 7)
let end = str.index(str.endIndex, offsetBy: -1)
let substring = str.substring(with: start..<end)
print(substring)

// Ausgabe: "Welt"
```

In diesem Beispiel definieren wir zuerst eine Variable `str`, die unseren Ausgangstext enthält. Dann geben wir den Startindex unseres Teilstrings mit Hilfe der `index()`-Funktion an. Hier wird der Index des Zeichens "W" im Wort "Welt" verwendet. Dann bestimmen wir den Endindex, indem wir die Länge des Textes (8) von der Gesamtlänge abziehen. Schließlich verwenden wir die `substring()`-Funktion, um den Teilstring zwischen dem Start- und Endindex zu extrahieren. In diesem Fall erhalten wir das Wort "Welt" als Ergebnis.

Eine weitere Möglichkeit besteht darin, den `split()`-Befehl zu verwenden, um einen String in Teilstrings anhand eines bestimmten Trennzeichens zu zerlegen. Hier ein Beispiel:

```Swift
let str = "Ich mag Äpfel, Birnen und Bananen."
let substrings = str.split(separator: ", ")
print(substrings)

// Ausgabe: ["Ich mag Äpfel", "Birnen und Bananen."]
```

In diesem Beispiel definieren wir zunächst einen String `str`, der verschiedene Früchte enthält und mit einem Komma und Leerzeichen getrennt ist. Mithilfe von `split()` wird dieser String in Teilstrings anhand des Trennzeichens zerlegt und in einem Array gespeichert.

Es gibt noch viele weitere Methoden, um Teilstrings in Swift zu extrahieren, wie z.B. die Verwendung von regulären Ausdrücken oder der `range(of:)`-Funktion. Es ist wichtig, dass wir uns mit diesen verschiedenen Techniken vertraut machen, um sie in unseren Projekten sinnvoll einzusetzen.

## Tiefen Einblick

Neben den oben genannten Methoden gibt es noch weitere Faktoren, die bei der Extrahierung von Teilstrings eine Rolle spielen. Zum Beispiel ist es wichtig zu beachten, dass die Indizierung von Zeichen in Strings in Swift mit `index()` und `offsetBy` entsprechend der verwendeten Zeichencodierung erfolgt. Daher kann es vorkommen, dass der Start- und Endindex je nach verwendeter Sprache oder Zeichensatz variieren können.

Ebenfalls wichtig ist es, mögliche Fehler bei der Extrahierung von Teilstrings zu vermeiden. Wenn der angegebene Index außerhalb des zugrunde liegenden Strings liegt, wird ein Fehler ausgelöst. Daher sollten wir immer sicherstellen, dass die angegebenen Indizes gültig sind, bevor wir versuchen, Teilstrings zu extrahieren.

Es lohnt sich auch, sich mit den verschiedenen Datenstrukturen in Swift, wie z.B. `Substring` und `String.Index`, auseinanderzusetzen, um komplexe Anwendungen mit Teilstrings zu realisieren.

## Siehe auch

- [Swift String Dokumentation](https://developer.apple.com/documentation/swift/string)
- [Strings and Characters in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift Programming Language - Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)