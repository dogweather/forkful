---
date: 2024-01-20 17:43:05.150936-07:00
description: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet,\
  \ bestimmte Teile eines Strings zu entfernen, basierend auf Regeln oder \"Mustern\"\
  .\u2026"
lastmod: '2024-03-13T22:44:54.210428-06:00'
model: gpt-4-1106-preview
summary: "Das L\xF6schen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte\
  \ Teile eines Strings zu entfernen, basierend auf Regeln oder \"Mustern\"."
title: "L\xF6schen von Zeichen, die einem Muster entsprechen"
weight: 5
---

## Vorgehensweise:
```Swift
var myString = "Hallo Welt! 123"
let pattern = "[0-9]"
if let regex = try? NSRegularExpression(pattern: pattern, options: []) {
    let range = NSRange(location: 0, length: myString.utf16.count)
    myString = regex.stringByReplacingMatches(in: myString, options: [], range: range, withTemplate: "")
}
print(myString)
// Ausgabe: Hallo Welt!
```
Hier entfernt das `NSRegularExpression` Objekt alle Zahlen aus unserem String.

## Vertiefung:
Die `NSRegularExpression` Klasse ist ein leistungsstarkes Tool für das Arbeiten mit regulären Ausdrücken in Swift, verfügbar seit iOS 4 und macOS 10.7. Alternativen dazu sind zum Beispiel die Verwendung von `String`-Methoden wie `replacingOccurrences(of:with:)`, allerdings ohne die Flexibilität von Mustern. Die Implementierung von `NSRegularExpression` basiert auf der ICU-Bibliothek, eine weitverbreitete Open-Source-Library für Unicode-Operationen.

## Weiterführende Links:
- Swift-Dokumentation von Apple zur `NSRegularExpression`: https://developer.apple.com/documentation/foundation/nsregularexpression
- ICU-Projektseite: http://site.icu-project.org/
- Swift String und Character Dokumentation: https://developer.apple.com/documentation/swift/string/
