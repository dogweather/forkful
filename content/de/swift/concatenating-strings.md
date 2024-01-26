---
title:                "Zeichenketten verknüpfen"
date:                  2024-01-20T17:35:38.699307-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
String-Konkatenation verbindet Textstücke zu einem Ganzen. Entwickler nutzen das, um dynamische Nachrichten zu bilden oder Text aus verschiedenen Quellen zusammenzuführen.

## How to:
Swift bietet mehrere Wege, Strings zu verketten:

```Swift
// Plus-Operator
let gruss = "Hallo, " + "Welt!"
print(gruss)  // "Hallo, Welt!"

// String-Interpolation
let name = "Swift"
let begruessung = "Willkommen, \(name)!"
print(begruessung)  // "Willkommen, Swift!"

// append()-Methode
var nachricht = "Frohes "
nachricht.append("Neues Jahr!")
print(nachricht)  // "Frohes Neues Jahr!"
```

## Deep Dive
String-Konkatenation gibt’s seit den Anfängen der Programmierung. Historisch gesehen nutzten Programmiersprachen wie C Operatoren wie `+` oder Funktionen wie `strcat()`. In Swift ist das Zusammensetzen von Strings durchgängig optimiert, dank `String` als strukturierter Datentyp. Alternativen zur Konkatenation sind Formatierungsfunktionen oder das Zusammensetzen von Arrays von Strings mit `joined()`:

```Swift
// joined()-Methode mit einem Array von Strings
let worte = ["Swift", "ist", "Spaß!"]
let satz = worte.joined(separator: " ")
print(satz)  // "Swift ist Spaß!"
```

In Swift werden Strings als Wertetypen gehandhabt. Konkatenation kann teuer sein, da sie oft zur Erstellung von völlig neuen String-Objekten führt.

## See Also
- [Swift-Dokumentation zu String und Zeichen](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Apple's Swift-API zu String](https://developer.apple.com/documentation/swift/string)
