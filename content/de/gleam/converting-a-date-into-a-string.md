---
title:                "Gleam: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Konvertierung von Daten in Strings ist ein wichtiger Schritt beim Entwickeln von Software. Durch die Umwandlung eines Datums in einen String können wir Daten in einem bestimmten Format darstellen und weiterverarbeiten. In diesem Beitrag werden wir uns die unterschiedlichen Möglichkeiten ansehen, wie man ein Datum in einem Gleam-Programm zu einem String umwandeln kann.

## Wie geht man vor?

Die Konvertierung eines Datums in Gleam ist dank der eingebauten Funktionen und Typen relativ einfach. Im Folgenden finden Sie einige Beispiele, die Ihnen den Einstieg erleichtern sollen:

```Gleam
import gleam/datetime as datetime

let date = datetime.Date.create(2021, 9, 12)

let string1 = datetime.Date.format("%d %B %Y", date)
let string2 = datetime.Date.to_iso_8601(date)
let string3 = datetime.Date.to_string(date)

// Output:
//
// string1 = "12 September 2021"
// string2 = "2021-09-12"
// string3 = "12-09-2021"
```

In dem obigen Beispiel verwenden wir die `format`-Funktion, um das Datum im gewünschten Format auszugeben. Wir können auch die Funktionen `to_iso_8601` und `to_string` verwenden, um das Datum in ein ISO-8601-konformes Format oder in das Gleam-Standardformat umzuwandeln.

## Tiefergehende Untersuchung

Die `format`-Funktion kann auch weitere Optionen für die Formatierung von Datum und Uhrzeit akzeptieren. Hier ist eine Tabelle mit einigen der häufigsten Optionen:

| Option  | Beschreibung                                    |
|---------|-------------------------------------------------|
| %Y      | 4-stellige Jahreszahl                           |
| %y      | 2-stellige Jahreszahl                           |
| %m      | Monat als 2-stellige Zahl                       |
| %B      | Monatsname (z.B. "September")                   |
| %b      | Abgekürzter Monatsname (z.B. "Sep")             |
| %d      | Tag des Monats als 2-stellige Zahl              |
| %A      | Wochentag (z.B. "Sonntag")                      |
| %a      | Abgekürzter Wochentag (z.B. "So")               |
| %H      | Stunde (24-Stunden-Format)                      |
| %I      | Stunde (12-Stunden-Format)                      |
| %M      | Minute                                          |
| %S      | Sekunde                                         |

Weitere Informationen zu den verschiedenen Formatierungsoptionen finden Sie in der Gleam-Dokumentation zu [DateTime.Format](https://gleam.run/language/datetime#DateTime.Format).

Es ist auch wichtig zu beachten, dass die Konvertierung von Datums- und Zeitangaben immer unter Berücksichtigung der Zeitzone erfolgt. Gleam bietet hierfür den Typ `DateTime.Offset` an, der verwendet werden kann, um Datum und Uhrzeit mit einer bestimmten Zeitzone zu verknüpfen.

## Siehe auch

- [Gleam-Dokumentation zu DateTime](https://gleam.run/language/datetime)
- [Blogbeitrag zum Thema Gleam-Datums- und Zeitmanipulation](https://devblog.avdi.codes/2021/02/23/gleam-dates-and-time/)