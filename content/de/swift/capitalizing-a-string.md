---
title:                "Swift: Einen String großschreiben"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Warum 
In diesem Blogbeitrag werden wir untersuchen, wie man in Swift eine Zeichenkette (String) großschreibt. Das Kapitalisieren von Strings ist ein häufig verwendeter Schritt in der Programmierung, da es hilft, Daten einheitlich zu formatieren und Lesbarkeit zu verbessern.

# Wie man es macht 
Um eine Zeichenkette in Swift zu kapitalisieren, gibt es zwei gängige Methoden: die Verwendung der `capitalize` Methode und die Verwendung der `uppercased` Methode. Hier ist ein Beispiel für beide Methoden:

```Swift
let name = "max mustermann"

// Verwendung der capitalize Methode
let capitalized1 = name.capitalize()
print(capitalized1)
// Ausgabe: Max mustermann

// Verwendung der uppercased Methode
let capitalized2 = name.uppercased()
print(capitalized2)
// Ausgabe: MAX MUSTERMANN

```

Wie Sie sehen können, gibt es einen kleinen Unterschied im Ergebnis der beiden Methoden. Die `capitalize` Methode behält die ursprüngliche Formatierung des Strings bei, während die `uppercased` Methode alle Buchstaben in Großbuchstaben ändert.

Sie können auch die Kombination von `uppercased` und `lowercased` verwenden, um bestimmte Buchstaben in einem String zu ändern. Hier ist ein Beispiel:

```Swift
let phrase = "Ich lEbe Swift!"

let uppercased = phrase.uppercased()
print(uppercased)
// Ausgabe: ICH LEBE SWIFT!

let lowercased = phrase.lowercased()
print(lowercased)
// Ausgabe: ich lebe swift!

let middleCapitalized = phrase[0] + uppercased[1..<uppercased.count - 1] + phrase[phrase.count - 1]
print(middleCapitalized)
// Ausgabe: ILebe Swift!

```

In diesem Beispiel haben wir die `uppercased` Methode verwendet, um alle Buchstaben in Großbuchstaben zu ändern, und dann die `lowercased` Methode, um alle Buchstaben in Kleinbuchstaben zu ändern. Schließlich haben wir die  `uppercased` und `lowercased` Ergebnisse kombiniert, um nur den mittleren Buchstaben großzuschreiben.

# Tiefere Einblicke 
Jetzt, wo wir wissen, wie wir eine Zeichenkette in Swift großschreiben können, werfen wir einen Blick auf die Grundlagen dessen, was im Hintergrund passiert.

Swift verwendet den Unicode-Standard für die Darstellung von Zeichenketten. Dies bedeutet, dass jeder Buchstabe, jede Zahl oder jedes Sonderzeichen einen eindeutigen Code hat, um es zu identifizieren. Wenn Sie also eine Zeichenkette großschreiben, ändert Swift nur die Unicode-Codes der Zeichen entsprechend.

Es ist auch wichtig zu beachten, dass die `uppercase` Methoden keine Auswirkungen auf Sonderzeichen haben. Sie funktionieren nur bei Buchstaben. Hier ist ein Beispiel:

```Swift
let str = "!!Hello!"
let capitalized = str.uppercased()
print(capitalized)
// Ausgabe: !!HELLO!

```

Das Ausrufezeichen wurde nicht in einen Großbuchstaben geändert, da es kein Buchstabe ist, sondern ein Sonderzeichen.

# Siehe auch
- [Apple Developer Dokumentation über String Manipulation (Englisch)](https://developer.apple.com/documentation/swift/string)
- [Swift-Blog Beitrag über Substring und String Manipulation (Englisch)](https://swift.org/blog/substring/)