---
date: 2024-01-20 17:48:08.011373-07:00
description: "In Swift bestimmt die L\xE4nge eines Strings, wie viele Zeichen er enth\xE4\
  lt. Das ist wichtig, weil viele Programmieraufgaben, wie die Validierung von Eingaben\u2026"
lastmod: '2024-03-11T00:14:28.119308-06:00'
model: gpt-4-1106-preview
summary: "In Swift bestimmt die L\xE4nge eines Strings, wie viele Zeichen er enth\xE4\
  lt. Das ist wichtig, weil viele Programmieraufgaben, wie die Validierung von Eingaben\u2026"
title: "Ermittlung der Zeichenkettenl\xE4nge"
---

{{< edit_this_page >}}

## Was & Warum?

In Swift bestimmt die Länge eines Strings, wie viele Zeichen er enthält. Das ist wichtig, weil viele Programmieraufgaben, wie die Validierung von Eingaben oder das Schneiden von Text, auf der Zeichenzahl basieren.

## So geht's:

Swift bietet eine einfache und direkte Art, die Länge eines Strings zu bekommen – über die `count` Eigenschaft. Hier ein Beispiel:

```swift
let beispielString = "Hallo Welt!"
print(beispielString.count)
```
Ausgabe: `11`

Beachte, dass Swift Unicode-Korrekt ist, was bedeutet, dass Emojis und kombinierte Zeichen als ein Zeichen gezählt werden:

```swift
let emojiString = "👨‍👩‍👧‍👦"
print(emojiString.count)
```
Ausgabe: `1`

## Deep Dive

Historisch gesehen waren Strings in manchen älteren Sprachen einfach Arrays von Zeichen, die mit einem Nullzeichen endeten. Die Länge zu finden, hieß, das Array zu durchlaufen, bis man dieses Endzeichen fand. In Swift sind Strings komplexer: Sie sind eine Sammlung von `Character` Werten, die eine Unicode-repräsentierende Abstraktion bieten. Swifts Ansatz erlaubt es, auch komplexe Zeichen richtig zu zählen.

Alternativen? In früheren Swift-Versionen oder anderen Programmiersprachen könntest du Methoden wie `length()` finden. In Swift ist `.count` aber der direkte Weg.

Die Implementierungsdetails zu kennen, bedeutet vor allem eines: Strings sind in Swift keine einfachen Char-Arrays. Deshalb ist die `count` Eigenschaft nicht einfach die Größe eines Arrays, sondern das Ergebnis eines Durchlaufs, der jede grapheme cluster überprüft – deshalb ist es effizient und korrekt.

## Siehe auch:

- [Swift Documentation on Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift String Manifesto](https://github.com/apple/swift/blob/main/docs/StringManifesto.md)
- [Unicode Standard](https://unicode.org/standard/standard.html)
