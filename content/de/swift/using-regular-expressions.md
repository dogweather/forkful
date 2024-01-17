---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Swift: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Was & Warum?
Regular Expressions sind eine Möglichkeit, Textmuster in Strings zu finden und zu manipulieren. Sie sind besonders nützlich für das Überprüfen von Eingaben oder das Extrahieren von Informationen aus großen Datenmengen. Programmierer nutzen sie, um effektiver und präziser mit Text zu interagieren.

# Wie?
### Überprüfen auf gültige E-Mail-Adresse:
```
let email = "johndoe@example.com"
let regex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,64}"
if email.range(of: regex, options: .regularExpression) != nil {
    print("Gültige E-Mail-Adresse.")
} else {
    print("Ungültige E-Mail-Adresse.")
}
```
Ausgabe: Gültige E-Mail-Adresse.

### Ersetzen von bestimmten Wörtern in einem String:
```
var sentence = "Ich mag Bananen und Äpfel."
let regex = "Bananen|Äpfel"
sentence = sentence.replacingOccurrences(of: regex, with: "Erdbeeren")
print(sentence)
```
Ausgabe: Ich mag Erdbeeren und Erdbeeren.

# Tiefer einsteigen
Regular Expressions gibt es schon seit den 50er Jahren und wurden vor allem in Unix-Systemen verwendet. Mittlerweile bieten viele Programmiersprachen, darunter auch Swift, eine eigene Syntax für Regular Expressions. Alternativ können auch String-Funktionen wie `contains()` oder `replacingOccurrences()` verwendet werden, die weniger komplex sind, aber auch weniger flexibel. Die Implementierung von Regular Expressions in Swift basiert auf dem ICU-Projekt.

# Siehe auch
Offizielle Swift-Dokumentation zu Regular Expressions: https://developer.apple.com/documentation/foundation/nsregularexpression
Tutorial zu Regular Expressions in Swift: https://www.raywenderlich.com/5766073-regular-expressions-tutorial-getting-started