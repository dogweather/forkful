---
title:                "Fehlerausgabe drucken"
html_title:           "Swift: Fehlerausgabe drucken"
simple_title:         "Fehlerausgabe drucken"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

# Was & Warum?

Debugging Output ausgeben ist einfach ein Weg für Programmierer, um ihre Programme zu überwachen und zu überprüfen. Es ist besonders nützlich, wenn man versucht, Probleme im Code zu finden und zu beheben. Durch das Drucken von Debug Ausgaben können Programmierer den Wert von Variablen und den Ablauf ihres Codes verfolgen, um Fehler besser zu verstehen.

# Wie geht's?

Das Drucken von Debug Ausgaben ist in Swift sehr einfach. Verwenden Sie einfach die Funktion `print()`, um eine Nachricht oder den Wert einer Variable auszugeben. Hier ist ein Beispiel:

```Swift
let name = "Max"
print("Hallo, ich bin \(name)")
```
Ausgabe: Hallo, ich bin Max

Sie können auch mehrere Werte in einer Debug Ausgabe zusammenfügen, indem Sie sie mit einem Komma trennen:

```Swift
let num = 5
print("Die Zahl ist", num)
```
Ausgabe: Die Zahl ist 5

# Tiefere Einblicke

Das Drucken von Debug Ausgaben wird schon seit den Anfängen der Programmierung verwendet, um Fehler zu finden und zu beheben. In Swift gibt es jedoch auch alternative Methoden, wie z.B. das Hinzufügen von Breakpoints oder das Loggen von Fehlern in einer Konsole.

Um eine Debug Ausgabe noch nützlicher zu machen, gibt es auch die Möglichkeit, benutzerdefinierte Funktionen zu erstellen, die spezielle Informationen ausgeben. Diese Funktionen können z.B. den Zustand des Programms zu einem bestimmten Zeitpunkt anzeigen oder spezifische Fehlermeldungen ausgeben.

# Siehe auch

Für weitere Informationen und Beispiele können Sie die offizielle Dokumentation von Apple über das Drucken von Debug Ausgaben in Swift lesen: [Debugging in Swift](https://developer.apple.com/documentation/swift/debugging).