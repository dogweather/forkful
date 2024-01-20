---
title:                "Strings verketten"
html_title:           "Bash: Strings verketten"
simple_title:         "Strings verketten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Als Programmierer verwenden wir die Zeichenkettenzusammenführung (String-Konkatenation), um zwei oder mehrere Zeichenketten zu einer einzigen Zeichenkette zusammenzusetzen. Es hilft uns dabei, dynamische Daten sauber zu formatieren und zu präsentieren.

## So geht's:

In Swift machst du die Zeichenkettenzusammenführung folgendermaßen:

```Swift
let string1 = "Hallo"
let string2 = "Welt"
let zusammen = string1 + " " + string2
print(zusammen)  // Ausgabe: "Hallo Welt"
```
Du kannst auch den zusammengesetzten Operator `+=` wie folgt verwenden:

```Swift
var willkommen = "Willkommen"
willkommen += " in Berlin"
print(willkommen)  // Ausgabe: "Willkommen in Berlin"
```
## Hintergrundwissen:

Frühere Versionen von Swift verwendeten die `stringByAppendingString:` Methode zur Zeichenkettenzusammenfügung, aber Swift hat diesen Prozess mit dem Operator '+' und '+=' für eine intuitivere und lesbarere Codeerfahrung vereinfacht. Es gibt jedoch immer Alternativen, wie die String-Interpolation, bei der Variablen direkt in Zeichenketten eingebettet werden können:

```Swift
let name = "Hans"
let begrüßung = "Hallo, \(name)"
print(begrüßung)  // Ausgabe: "Hallo, Hans"
```
Bei der String-Konkatenation in Swift ist zu beachten, dass jede Konkatenation von Zeichenketten tatsächlich eine neue Zeichenkette generiert. Swift's Strings sind vielzahlunabhängig (Unicode-compliant), was bedeutet, dass sie mehrere Zeichenarten, einschließlich Emoji, unterstützen.

## Weiterführende Informationen:

- Die offizielle Swift-Dokumentation zu Strings und Zeichen findest du unter: [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Für eine tiefere Diskussion über die Effizienz von String-Konkatenationen in Swift, sieh dir diesen Beitrag auf StackOverflow an: [Stack Overflow Discussion](https://stackoverflow.com/questions/24200888/any-performance-benefit-to-constant-strings-in-swift) 
- Um mehr über die verschiedenen Methoden zur Arbeit mit Strings in Swift zu erfahren, sieh dir diesen Leitfaden an: [Raywenderlich Swift Strings Guide](https://www.raywenderlich.com/4492-strings-in-swift-4)