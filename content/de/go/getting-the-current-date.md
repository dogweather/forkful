---
title:                "Go: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine häufige Aufgabe beim Programmieren. Es kann genutzt werden, um Berichte zu erstellen, Kalenderfunktionen zu implementieren oder einfach um das aktuelle Datum in einer Anwendung anzuzeigen.

## Wie geht's

Um das aktuelle Datum in Go zu bekommen, können wir die Funktion `Now()` aus dem Package `time` nutzen.

```
Go now := time.Now()
fmt.Println(now)
```

Dieser Code gibt das aktuelle Datum und die Uhrzeit in der Ausgabe aus.

```
Output:
2021-10-12 15:30:00
```

Um das Datum in einem bestimmten Format anzuzeigen, können wir die `Format()` Funktion verwenden. Hier ist ein Beispiel, das das Datum im Format "02.01.2006" (Tag.Monat.Jahr) ausgibt.

```
Go now := time.Now()
fmt.Println(now.Format("02.01.2006"))
```

Dieser Code gibt das aktuelle Datum in der Ausgabe aus:

```
Output:
12.10.2021
```

## Tiefergehende Informationen

Die Funktion `Now()` gibt das Datum und die Uhrzeit in der Zeitzone zurück, in der das Programm gerade ausgeführt wird. Wenn wir das Datum in einer anderen Zeitzone abrufen möchten, können wir das `Time` Objekt mit der Funktion `In()` auf die gewünschte Zeitzone konvertieren. Zum Beispiel:

```
Go now := time.Now()
nyTimeZone, _ := time.LoadLocation("America/New_York")
nyTime := now.In(nyTimeZone)
fmt.Println(nyTime.Format("02.01.2006"))
```

Dieses Beispiel gibt das aktuelle Datum in New York aus, unabhängig davon, wo das Programm ausgeführt wird.

Es ist auch möglich, bestimmte Datumsangaben wie Tage, Monate oder Jahre hinzuzufügen oder zu subtrahieren. Dafür können wir die Funktionen `Add()` oder `Sub()` verwenden. Beispiel:

```
Go now := time.Now()
inTwoDays := now.AddDate(0, 0, 2)
fmt.Println(inTwoDays.Format("02.01.2006"))
```

Dieser Code gibt das Datum aus, welches zwei Tage in der Zukunft liegt.

## Siehe auch

- Offizielle Dokumentation zu `time`: https://golang.org/pkg/time/
- Tutorial zu Datum und Zeit in Go: https://tutorialedge.net/golang/go-date-time-tutorial/
- Zeitzone Paket `time/location`: https://pkg.go.dev/time/location