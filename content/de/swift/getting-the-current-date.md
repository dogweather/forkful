---
title:                "Die aktuelle Datum abrufen"
html_title:           "Swift: Die aktuelle Datum abrufen"
simple_title:         "Die aktuelle Datum abrufen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe beim Programmieren in Swift. Es kann verwendet werden, um zeitbasierte Funktionen zu implementieren, wie z.B. die Anzeige des aktuellen Datums in einer App oder das Erstellen von Dateinamen mit dem aktuellen Datum.

## Wie geht das?

Die einfachste Möglichkeit, das aktuelle Datum in Swift zu erhalten, ist die Verwendung der `Date()`-Klasse. Mit dieser Klasse können wir das aktuelle Datum und die Uhrzeit zusammen mit anderen nützlichen Funktionen wie das Formatieren des Datums abrufen.

```Swift
let currentDate = Date() // Das aktuelle Datum und Uhrzeit abrufen
let dateFormatter = DateFormatter() // Einen DateFormatter erstellen
dateFormatter.dateFormat = "dd.MM.yyyy" // Das gewünschte Datumsformat festlegen
let formattedDate = dateFormatter.string(from: currentDate) // Das Datum formatieren
print(formattedDate) // 24.05.2021
```

Wir können auch die `Calendar`-Klasse verwenden, um das Datum in bestimmten Zeitzonen oder Kalendersystemen abzurufen.

```Swift
let calendar = Calendar(identifier: .gregorian) // Den gregorianischen Kalender verwenden
let currentDate = Date()
let components = calendar.dateComponents([.day, .month, .year], from: currentDate) // Die Teile des Datums abrufen
print(components.day!) // 24
print(components.month!) // 05
print(components.year!) // 2021
```

## Tiefergehende Informationen

Die `Date()`-Klasse basiert auf der GMT-Zeitzone, daher kann es erforderlich sein, diese umzuwandeln, wenn das Ergebnis des Datums in einer anderen Zeitzone angezeigt werden soll. Wir können dies tun, indem wir die `TimeZone`-Klasse verwenden und diese in den `DateFormatter` einbinden.

```Swift
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.timeZone = TimeZone(identifier: "Europe/Berlin") // Die Zeitzone konvertieren
dateFormatter.dateFormat = "dd.MM.yyyy" 
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate) // 24.05.2021
```

Es ist auch möglich, das aktuelle Datum und die Uhrzeit mit einer Zeitdifferenz zu berechnen, z.B. um ein Datum in der Zukunft oder Vergangenheit zu erhalten.

```Swift
let currentDate = Date()
let calendar = Calendar(identifier: .gregorian)
let futureDate = calendar.date(byAdding: .day, value: 7, to: currentDate) // Das Datum um 7 Tage in die Zukunft setzen
let pastDate = calendar.date(byAdding: .month, value: -1, to: currentDate) // Das Datum um einen Monat in die Vergangenheit setzen
```

## Siehe auch

- [Apple Dokumentation zu Date](https://developer.apple.com/documentation/foundation/date)
- [WWDC Video "What's new in Foundation"](https://developer.apple.com/videos/play/wwdc2020/10170)