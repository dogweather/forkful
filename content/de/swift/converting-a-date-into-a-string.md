---
title:    "Swift: Eine Datumsangabe in einen String umwandeln"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Daten in Strings ist eine grundlegende und häufig verwendete Funktion in der Swift-Programmierung. Es ermöglicht es Entwicklern, Datumsangaben in verschiedenen Formaten darzustellen und somit eine bessere Nutzererfahrung zu schaffen.

## Wie geht das?

Um ein Datum in einen String umzuwandeln, gibt es verschiedene Methoden, abhängig von der gewünschten Ausgabe. Hier sind einige Beispiele:

```Swift
// Konvertierung in einen Standard-String mit Datum und Uhrzeit
let date = Date()
let string = DateFormatter.localizedString(from: date, dateStyle: .medium, timeStyle: .short)
print(string) // 23. August 2021 um 15:30:45

// Konvertierung in einen individuellen String mit benutzerdefiniertem Format
let customFormatter = DateFormatter()
customFormatter.dateFormat = "dd.MM.yyyy"
let dateString = customFormatter.string(from: date)
print(dateString) // 23.08.2021
```

Bei Verwendung von `DateFormatter` ist es wichtig, die `locale`-Eigenschaft zu beachten, da dies die Formatierung beeinflussen kann.

## Tiefergehende Informationen

Wenn wir uns einmal genauer anschauen, wie das Konvertieren eines Datums in einen String funktioniert, sehen wir, dass es im Grunde genommen eine Umwandlung von einem `Date`-Objekt in einen `String`-Datentyp ist. Dazu werden verschiedenen Methoden im `DateFormatter`-Objekt verwendet, um das Datum entsprechend zu formatieren.

Ein weiterer wichtiger Aspekt bei der Konvertierung von Dateien in Strings ist die Berücksichtigung von verschiedenen Zeitzonen. Hier kann die `timeZone`-Eigenschaft im `DateFormatter` verwendet werden, um das Datum entsprechend an die gewünschte Zeitzone anzupassen.

## Siehe auch

- [Apple Documentation: Date Formatting Guide](https://developer.apple.com/documentation/foundation/date)
- [Hacking with Swift: Date formatting made easy](https://www.hackingwithswift.com/articles/104/how-to-format-dates-with-dateformatter-on-ios)
- [DevTo: Converting dates to strings in Swift](https://dev.to/sandordargo/converting-dates-to-strings-in-swift-1799)