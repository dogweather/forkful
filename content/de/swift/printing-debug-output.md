---
title:                "Debug-Ausgaben drucken"
date:                  2024-01-20T17:53:25.471457-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"

category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Printing debug output ist das Ausgeben von Nachrichten in der Konsole, um zu verfolgen, was im Code passiert. Programmierer verwenden es, um Fehler zu finden und zu verstehen, wie ihr Code während der Ausführung funktioniert.

## So geht's:
Um etwas in der Konsole auszugeben, nutzen wir `print()` oder `debugPrint()`. Hier ein Beispiel:
```Swift
let name = "Welt"
print("Hallo, \(name)!")
// Ausgabe: Hallo, Welt!

debugPrint("Hallo, \(name)!")
// Ausgabe: "Hallo, Welt!"
```
Die Funktion `debugPrint()` ist hilfreich, wenn wir eine detailliertere Ausgabe für Debug-Zwecke wollen.

## Tiefere Einblicke:
Die `print()`-Funktion gibt es seit den Anfängen von Swift. Alternativen zu `print()` sind beispielsweise `NSLog()`, das mehr Kontext bietet, aber langsamer ist. Eine weitere Möglichkeit ist die Verwendung von Logging-Frameworks wie `os_log`, die in Apples Betriebssystemen integriert sind und eine konfigurierbare Log-Ebene bieten.

Debug Output sollte nicht in Produktionscode verbleiben, da es Performance beeinträchtigen kann und potenziell sensible Informationen preisgibt. Es ist sinnvoll, die Logs bei der Veröffentlichung einer App zu entfernen oder auf ein Minimum zu reduzieren.

## Siehe auch:
- Apples Swift Logging API: https://developer.apple.com/documentation/os/logging
- Swift Dokumentation über `print()`: https://developer.apple.com/documentation/swift/1541053-print
- Swift Dokumentation über `debugPrint()`: https://developer.apple.com/documentation/swift/1539127-debugprint
