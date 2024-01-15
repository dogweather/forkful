---
title:                "Das aktuelle Datum erhalten"
html_title:           "Go: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Im heutigen technologiegetriebenen Zeitalter ist es wichtig, das aktuelle Datum und die Uhrzeit in verschiedenen Anwendungen und Projekten zu kennen. Ob für die Datenaufzeichnung, die Terminplanung oder das Verwalten von Dateinamen, das Abrufen des aktuellen Datums ist in der Programmierung unerlässlich.

## Wie geht das

Um das aktuelle Datum in Go zu bekommen, können wir die Funktion "Now" aus der Paketbibliothek "time" nutzen. Diese Funktion gibt ein "time.Time" Objekt zurück, das das aktuelle Datum und die Uhrzeit speichert.

```Go
import "time"

currentDate := time.Now() // currentDate ist vom Typ "time.Time"
fmt.Println(currentDate) // Ausgabe: 2021-11-07 09:14:32.4159697 +0000 UTC m=+0.004000001
```

Die Ausgabe zeigt das Datum, die Uhrzeit sowie die Zeitzone des aktuellen Zeitpunkts. Wir können auch auf bestimmte Komponenten des Objekts zugreifen, wie z.B. den Tag oder die Stunde.

```Go
currentDay := currentDate.Day()
currentHour := currentDate.Hour()

fmt.Println(currentDay) // Ausgabe: 7
fmt.Println(currentHour) // Ausgabe: 9
```

## Tief eintauchen

Das "time" Paket bietet noch weitere Funktionen, um das Datum und die Uhrzeit zu formatieren oder zu vergleichen. Es gibt auch Möglichkeiten, die Zeitzone und die Lokalisierung anzupassen.

Ein wichtiger Hinweis ist, dass das "time.Time" Objekt auf eine "null" Zeitzone gesetzt ist. Wenn Sie also mit verschiedenen Zeitzonen arbeiten müssen, sollten Sie die Funktion "time.LoadLocation" nutzen, um die entsprechende Zeitzone zu laden.

```Go
// Zeitzone setzen
loc, _ := time.LoadLocation("Europe/Berlin")
currentTime := time.Now().In(loc)

fmt.Println(currentTime) // Ausgabe: 2021-11-07 10:14:32.4159697 +0100 CET
```

Schauen Sie sich die Dokumentation des "time" Pakets an, um alle verfügbaren Funktionen und Anpassungsmöglichkeiten zu entdecken.

## Siehe auch

- Go Zeitpaket Dokumentation: https://pkg.go.dev/time
- Weitere Go Artikel und Tutorials: https://www.golang.org