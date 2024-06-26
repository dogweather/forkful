---
date: 2024-01-20 17:51:57.174846-07:00
description: "So geht's: In der Geschichte von Swift wurde String-Interpolation mit\
  \ der ersten Version des Sprache eingef\xFChrt und ist seitdem eine Grundfunktion.\
  \ Es\u2026"
lastmod: '2024-04-05T21:53:56.098589-06:00'
model: gpt-4-1106-preview
summary: "In der Geschichte von Swift wurde String-Interpolation mit der ersten Version\
  \ des Sprache eingef\xFChrt und ist seitdem eine Grundfunktion."
title: Zeichenketten interpolieren
weight: 8
---

## So geht's:
```Swift
let name = "Anna"
let age = 28
let greeting = "Hallo, ich bin \(name) und ich bin \(age) Jahre alt."
print(greeting)
```
Output:
```
Hallo, ich bin Anna und ich bin 28 Jahre alt.
```

Erweitertes Beispiel mit Berechnungen:
```Swift
let apples = 3
let oranges = 5
let fruitSummary = "Ich habe \(apples + oranges) Früchte insgesamt."
print(fruitSummary)
```
Output:
```
Ich habe 8 Früchte insgesamt.
```

## Tiefgang
In der Geschichte von Swift wurde String-Interpolation mit der ersten Version des Sprache eingeführt und ist seitdem eine Grundfunktion. Es unterscheidet sich von älteren Methoden wie Printf- oder Format-Strings, die in Sprachen wie C oder Java genutzt werden, indem es eine einfachere und sicherere Syntax bietet. Alternativ könnte man Strings durch Verkettung von Teilen mit dem `+` Operator zusammenstellen, was aber meist unübersichtlicher und fehleranfälliger ist.

Die String-Interpolation in Swift verwendet die Syntax `\(ausdruck)`, wobei `ausdruck` eine Variable, eine Konstante oder sogar eine Berechnung sein kann. Bei der Kompilierung setzt Swift den Wert des Ausdrucks in den String ein. Interessanterweise ist die Funktionalität von "\(…)" tatsächlich durch das Protokoll `CustomStringConvertible` implementiert, das die `description`-Eigenschaft für den Ausdruck liefert, den man interpolieren möchte.

## Siehe Auch
- Swift Documentation on Strings: [docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
