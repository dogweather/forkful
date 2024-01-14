---
title:    "Swift: Eine Zeichenkette großschreiben"
keywords: ["Swift"]
---

{{< edit_this_page >}}

#Warum

Das Kapitalisieren einer Zeichenfolge ist ein grundlegender Ansatz in der Programmierung, der in vielen Situationen nützlich sein kann. Zum Beispiel könnte es verwendet werden, um Eingaben von Benutzern unabhängig von der Groß- und Kleinschreibung zu lesen oder um Text für die Benutzeroberfläche zu formatieren.

#Wie

Die Swift Standardbibliothek hat eine funktionale Technik, um eine Zeichenfolge zu kapitalisieren, indem Sie die `capitalized` Eigenschaft auf eine Zeichenfolge anwenden:

```Swift
let meinText = "hallo welt!"
print(meinText.capitalized)
// Ausgabe: Hallo Welt!
```

Wenn Sie jedoch den Fall jeder einzelnen Zeichenfolge in einer Zeichenfolgenreihe kontrollieren möchten, können Sie die `map` Funktion verwenden, um jede Zeichenfolge in klein- oder großbuchstaben umzuwandeln:

```Swift
let meinText = "Hallo Welt"
let groBeBuchstaben = meinText.map{$0.uppercased()}
let kleineBuchstaben = meinText.map{$0.lowercased()}

print(groBeBuchstaben)
// Ausgabe: HALLO WELT
print(kleineBuchstaben)
// Ausgabe: hallo welt
```

#Tiefer Einblick

Eine Zeichenfolge in Swift wird als Datentyp im `String` struct dargestellt. Rufen Sie die `capitalized` Eigenschaft auf dieser Zeichenfolge ab, gibt sie eine neue Zeichenfolge zurück, die den ersten Buchstaben jedes Wortes in der Zeichenfolge groß schreibt. Dies geschieht mithilfe der Unicode-Kategorie "Letter, Titlecase".

Es gibt auch andere Kapitalisierungsmethoden wie `uppercased`, `lowercased` und `capitalized(with:)`, die eine Zeichenfolge entsprechend der angegebenen Formatierung umwandeln.

#Siehe auch

- [Die Swift Standardbibliothek](https://developer.apple.com/documentation/swift)
- [Apple's Swift Blog](https://developer.apple.com/swift/blog/)