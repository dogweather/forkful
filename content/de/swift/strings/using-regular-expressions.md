---
title:                "Reguläre Ausdrücke verwenden"
aliases:
- /de/swift/using-regular-expressions.md
date:                  2024-02-03T19:18:35.370076-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguläre Ausdrücke verwenden"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke oder Regex sind Zeichenfolgen, die ein Suchmuster bilden und häufig für die Übereinstimmung oder Manipulation von Zeichenketten verwendet werden. Programmierer nutzen sie für alles von der Datenvalidierung und -analyse bis zu Transformationen, was sie zu einem unverzichtbaren Werkzeug bei der Textverarbeitung und -manipulation in verschiedenen Programmiersprachen, einschließlich Swift, macht.

## Wie:
Die native Unterstützung von Swift für Regex verwendet die Klasse `NSRegularExpression` zusammen mit den Bereichs- und Ersetzungsmethoden der String-Klasse. Unten ist ein Beispiel für die Verwendung von Regex, um E-Mail-Adressen innerhalb eines Textblocks zu finden und hervorzuheben:

```swift
import Foundation

let text = "Kontaktieren Sie uns unter support@example.com oder feedback@example.org für weitere Informationen."
let regexMuster = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

do {
    let regex = try NSRegularExpression(pattern: regexMuster)
    let matches = regex.matches(in: text, range: NSRange(text.startIndex..., in: text))

    if !matches.isEmpty {
        for match in matches {
            let range = Range(match.range, in: text)!
            print("Gefunden: \(text[range])")
        }
    } else {
        print("Keine Treffer gefunden.")
    }
} catch {
    print("Regex-Fehler: \(error.localizedDescription)")
}

// Beispiel-Ausgabe:
// Gefunden: support@example.com
// Gefunden: feedback@example.org
```

Für komplexere oder bequemere Szenarien können Sie Drittanbieter-Bibliotheken wie SwiftRegex verwenden, die die Syntax vereinfachen und die Möglichkeiten erweitern. Obwohl Swifts Standardbibliothek leistungsfähig ist, bevorzugen einige Entwickler diese Bibliotheken für ihre prägnante Syntax und zusätzlichen Funktionen. So könnten Sie eine ähnliche Aufgabe mit einer hypothetischen Drittanbieter-Bibliothek ausführen:

```swift
// Angenommen, eine Bibliothek namens SwiftRegex existiert und ist importiert
let text = "Kontaktieren Sie uns unter hello@world.com oder besuchen Sie unsere Webseite."
let emailMuster = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

let emails = text.matches(for: emailMuster) // Hypothetische Methode, die von SwiftRegex bereitgestellt wird
if emails.isEmpty {
    print("Keine E-Mail-Adressen gefunden.")
} else {
    emails.forEach { email in
        print("Gefunden: \(email)")
    }
}

// Hypothetische Ausgabe, angenommen die `matches(for:)` Methode existiert in SwiftRegex:
// Gefunden: hello@world.com
```

Dieses Beispiel illustriert die Verwendung eines Drittanbieter-Regulär-Ausdruck-Pakets zur Vereinfachung der Suche nach Übereinstimmungen innerhalb einer Zeichenkette, unter der Annahme, dass solche Bequemlichkeitsmethoden wie `matches(for:)` existieren. Es ist wichtig, sich auf die jeweilige Drittanbieter-Bibliotheksdokumentation für genaue Syntax und Methodenverfügbarkeit zu beziehen.
