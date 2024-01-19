---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Druckausgabe von Debug-Informationen ist ein Vorgang, bei dem die Interna eines Programms auf einfache Weise angezeigt werden. Programmierer nutzen dies oft, um Fehler im Code aufzuspüren und Datenflüsse besser zu verstehen.

## Wie macht man das?

In Swift gibt es die eingebaute Funktion `print()`, um Debug-Ausgaben zu erzeugen. Hier sind einige Beispiele:

```Swift
let name = "Lukas"
print(name)
// Ausgabe: Lukas

let number = 24
print("Meine Zahl ist \(number).")
// Ausgabe: Meine Zahl ist 24.

print("Wenn man hier ankommt, ist etwas schief gelaufen.")
// Ausgabe: Wenn man hier ankommt, ist etwas schief gelaufen.
```

## Vertiefung

Während der Entwicklung von Swift wurde der Bedarf an starken Debugging-Werkzeugen wie `print()` früh erkannt. Historisch gesehen waren Ausgaben zur Fehlerbehebung bereits in den Anfängen der Programmiersprachen präsent.

Eine Alternative zu `print()` ist die Funktion `debugPrint()`. Sie liefert detailliertere Ausgaben, einschließlich der Namen von Enum-Case-Konstanten und Variablen:

```Swift
var age = 24
debugPrint("Alter ist \(age).")
// Ausgabe: "Alter ist 24.\n"
```

Interessanterweise implementiert Swift das Drucken von Debug-Ausgaben durch Umleitung der Standardausgabe ("stdout"). Dies ermöglicht es, Ausgaben auch an andere Orte als die Konsole zu senden, wie zum Beispiel Dateien.

## Siehe auch

Hier sind einige Ressourcen, wenn du dich weiter mit dem Thema beschäftigen willst:

- "Swift by Sundell": [What's the difference between print, dump and debugPrint in Swift](https://www.swiftbysundell.com/articles/debugging-in-swift/)
- "Hacking with Swift": [Printing debug text with print()](https://www.hackingwithswift.com/read/0/3/printing-debug-text-with-print)
- Apple's Dokumentation: [Debugging with Swift](https://developer.apple.com/documentation/swift/cocoa_design_patterns/debugging-and-reflection)