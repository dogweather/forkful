---
date: 2024-01-20 17:46:39.785379-07:00
description: 'So geht''s: Beispiel zum Extrahieren eines Wortes an einer bestimmten
  Position.'
lastmod: '2024-04-05T21:53:56.102045-06:00'
model: gpt-4-1106-preview
summary: Beispiel zum Extrahieren eines Wortes an einer bestimmten Position.
title: Teilstrings extrahieren
weight: 6
---

## So geht's:
```Swift
let vollerText = "Hallo, ich bin ein Swift-Programmierer!"
let bereich = vollerText.index(vollerText.startIndex, offsetBy: 7)..<vollerText.index(vollerText.endIndex, offsetBy: -23)
let teilString = vollerText[bereich] // "ich bin"

print(teilString) // Gibt "ich bin" aus
```

Beispiel zum Extrahieren eines Wortes an einer bestimmten Position:

```Swift
let satz = "Ein Apfel täglich hält den Doktor fern."
if let wortBereich = satz.range(of: "Apfel") {
    let wort = satz[wortBereich]
    print(wort) // Gibt "Apfel" aus
}
```

## Tiefere Einblicke
Vor Swift 4 wurden Teilstrings nicht als `Substring` gehandhabt, sondern direkt als `String`, was zu unnötigem Kopieren und Speichernutzung führte. Der `Substring` Typ in Swift bietet eine performante Möglichkeit, mit Teilstrings zu arbeiten, ohne sofort neue String-Instanzen zu erzeugen. Alternativ kann man auch mit Regular Expressions oder Funktionen wie `components(separatedBy:)` arbeiten.

Die Implementierung nutzt Copy-On-Write (CoW), was bedeutet, dass Speicher erst dann kopiert wird, wenn Änderungen vorgenommen werden müssen. Das ist effizient, da es vermeidet, dass große Datenmengen unnötigerweise kopiert werden, wenn man nur auf Teile des Originalstrings verweisen möchte.

## Siehe auch
- Swift Standard Library: [Strings and Characters](https://developer.apple.com/documentation/swift/string) 
- Swift Book von Apple: [String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Tutorial zu Regular Expressions in Swift: [NSRegularExpression](https://nshipster.com/nspredicate/)
