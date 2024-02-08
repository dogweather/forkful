---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
aliases:
- de/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:05.150936-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Löschen von Zeichen, die einem Muster entsprechen, bedeutet, bestimmte Teile eines Strings zu entfernen, basierend auf Regeln oder "Mustern". Programmer tun dies für Datensäuberung, Formatierung oder wenn nur bestimmte Dateninhalte benötigt werden.

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
