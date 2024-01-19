---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was und Warum?

Die Umwandlung eines Datums in einen String in Swift gewährt uns mehr Flexibilität bei der Gestaltung und Anzeige des Datumsformats. Programmierer tun dies, um die Datumsdaten für den Benutzer in einem lesbareren Format darzustellen.

## Wie es geht:

Wir können die Datumsausgabe in Swift mithilfe des DateFormatter-Objekts formatieren. Hier ist ein einfaches Beispiel:

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateStyle = .long
let dateString = formatter.string(from: date)
print(dateString)
```
In diesem Code wird das aktuelle Datum und Zeit in einem langen Stil angezeigt, ähnlich wie "22. Januar 2022."

## Tiefgang:

Historisch gesehen gibt es viele Möglichkeiten, ein Datum in eine Zeichenkette umzuwandeln. In früheren Versionen von Swift und anderen Programmiersprachen waren hierfür möglicherweise kompliziertere Ansätze erforderlich. 

Als Alternative können Sie das Datumsformat manuell festlegen, das mehr Kontrolle über das Format bietet. Hier können Sie beispielsweise das Jahr-Monat-Tag-Format verwenden:

```Swift
formatter.dateFormat = "yyyy-MM-dd"
let dateString = formatter.string(from: date)
print(dateString)
```
Dieser Code würde "2022-01-22" ausgeben.

Beim Implementieren dieser Funktion können Probleme mit Zeitzonen und Lokalisierungen auftreten. Swift behandelt dies jedoch gut, indem es verschiedene Stile und Formate für verschiedene Lokalisierungen anbietet.

## Weitere Info:

- Swift Dokumentation: DateFormatter: [Link](https://developer.apple.com/documentation/foundation/dateformatter)
- Apple Developer: Dates and Times: [Link](https://developer.apple.com/documentation/foundation/dates_and_times)
- Swift Tutorial: Wie formatiere ich Daten und Zeiten in Swift?: [Link](https://www.hackingwithswift.com/example-code/strings/how-to-format-dates-and-times-using-dateformatter)