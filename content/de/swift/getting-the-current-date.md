---
title:    "Swift: Das heutige Datum erhalten"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man das aktuelle Datum in Swift erhalten kann. Das ist eine essentielle Funktion, die in vielen Anwendungen benötigt wird. Wir werden sehen, wie einfach es ist, das aktuelle Datum in Swift zu bekommen und wie man es in verschiedenen Formaten anzeigen kann.

## Wie man das aktuelle Datum erhält

Die Verwendung von Swift-Befehlen ist ein nützlicher Weg, um das aktuelle Datum in einer Anwendung zu bekommen. Dazu muss man lediglich die `Date()` Funktion verwenden und sie einer Variablen zuweisen:

```Swift
let currentDate = Date()
```

Mit dieser Variablen kann man nun auf verschiedene Funktionen von Swift zugreifen, um das Datum in verschiedenen Formaten zu erhalten. Zum Beispiel:

```Swift
// aktuelles Datum als String DD.MM.YYYY
let currentDateAsString = currentDate.toString(format: "dd.MM.yyyy")

// aktuelles Datum als String mit Tag und Monatsnamen
let currentDateWithNames = currentDate.toString(format: "EEE, MMMM dd, yyyy")
```

Man kann auch auf spezifische Teile des Datums zugreifen, wie Tag, Monat und Jahr, und diese in Variablen speichern:

```Swift
let day = currentDate.getDay()
let month = currentDate.getMonth()
let year = currentDate.getYear()
```

Es gibt auch die Möglichkeit, das aktuelle Datum in einer bestimmten Zeitzone zu erhalten. Dazu muss man nur den `TimeZone` Parameter in der `Date()` Funktion angeben:

```Swift
// aktuelles Datum in der Zeitzone Deutschland
let currentDateInGermany = Date(timeIntervalSinceNow: TimeZone(identifier: "Europe/Berlin")!.secondsFromGMT())
```

## Tiefergehende Informationen

Die `Date()` Funktion ist in der Lage, das Datum aufgrund der Systemzeit des Geräts zu erhalten. Dabei ist es wichtig zu beachten, dass die Systemzeit des Geräts möglicherweise nicht immer korrekt ist. Dies kann auf verschiedene Faktoren wie beispielsweise eine falsche Zeiteinstellung des Benutzers zurückzuführen sein. Deshalb ist es ratsam, das Datum mit Vorsicht zu verwenden und im Zweifelsfall eine alternative Quelle zu nutzen.

## Siehe auch

- [Apple Dokumentation zu Date](https://developer.apple.com/documentation/foundation/date)
- [Stack Overflow Frage zu Zeitzone in Swift](https://stackoverflow.com/questions/25315484/convert-utc-date-into-local-time-in-swift)