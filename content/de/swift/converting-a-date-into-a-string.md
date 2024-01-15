---
title:                "Ein Datum in eine Zeichenkette umwandeln"
html_title:           "Swift: Ein Datum in eine Zeichenkette umwandeln"
simple_title:         "Ein Datum in eine Zeichenkette umwandeln"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum
Hast du dich jemals gefragt, wie man ein Datum in einen String umwandeln kann? Vielleicht hast du ein Datum in deiner App, das du dem Benutzer anzeigen möchtest, aber du musst es in einen lesbaren Text umwandeln. In Swift gibt es eine einfache Möglichkeit, dies zu tun, und in diesem Artikel werden wir uns ansehen, wie du das machen kannst.

## Wie es geht
```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dateString = formatter.string(from: date)
print(dateString) // Ausgabe: 16.03.2021
```

Der erste Schritt ist, ein Date-Objekt zu erstellen, das das aktuelle Datum und die aktuelle Uhrzeit enthält. Dann verwenden wir einen DateFormatter, um das Format des Datums zu definieren, das wir in unseren String umwandeln möchten. In diesem Beispiel haben wir das Format "dd.MM.yyyy" verwendet, was bedeutet, dass der Tag zuerst kommt, gefolgt vom Monat und dem Jahr. Zum Schluss rufen wir die Methode "string" des DateFormatter-Objekts auf und übergeben das Datum, das wir konvertieren möchten. Der resultierende String wird dann in die Variable "dateString" gespeichert und kann beliebig verwendet werden.

## Deep Dive
Wenn du dich genauer mit dem DateFormatter beschäftigst, wirst du feststellen, dass es viele verschiedene Optionen gibt, um das Datum zu formatieren. Zum Beispiel könntest du das Zeitformat hinzufügen, um die Uhrzeit mit anzugeben, oder das Format des Monats ändern, so dass er als Wort anstelle von einer Zahl angezeigt wird. Der DateFormatter hat auch die Fähigkeit, Sprach-, Kalender- und Zeitzone-Einstellungen zu berücksichtigen, so dass du auch internationalisierte Datumsangaben in deiner App verwenden kannst.

## Siehe auch
- [Apple Developer Documentation: DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift.org: Date and Time](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)