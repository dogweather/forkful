---
title:                "Konvertieren eines Datums in einen String"
html_title:           "Go: Konvertieren eines Datums in einen String"
simple_title:         "Konvertieren eines Datums in einen String"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum: Daten in einen String konvertieren?

Die Konvertierung von Daten in einen String ist in der Programmierung häufig erforderlich, insbesondere wenn es darum geht, Daten anzuzeigen oder zu speichern. In Go kann dies mithilfe der Funktionen des "time" Pakets erreicht werden, die es ermöglichen, ein Datum in verschiedene String-Formate zu formatieren.

## So geht's: Beispiele und Ausgabe

Um ein Datum in einen String umzuwandeln, benötigen wir zunächst ein "time" Objekt. Wir können dies mithilfe  der Funktion "Now ()" erstellen, die das aktuelle Datum und die aktuelle Uhrzeit zurückgibt. Anschließend können wir die "Format()" Funktion verwenden, um das Datum in einen String in einem bestimmten Format zu konvertieren. Hier ist ein Beispiel, das das Datum im Format "2006-01-02" ausgibt:

```Go
t := time.Now()
dateString := t.Format("2006-01-02")

fmt.Println(dateString) // Ausgabe: 2021-07-25
```

Neben dem Datum können wir auch die Uhrzeit in verschiedenen Formaten ausgeben. Hier ist ein weiteres Beispiel, das die Uhrzeit im Format "15:04" ausgibt:

```Go
t := time.Now()
timeString := t.Format("15:04")

fmt.Println(timeString) // Ausgabe: 12:30
```

## Tiefer eintauchen: Weitere Informationen

Für eine detaillierte Auflistung aller möglichen Formate zum Konvertieren von Daten in einen String, können Sie die offizielle Dokumentation des "time" Pakets hier [https://pkg.go.dev/time#Time.Format] einsehen. Es ist wichtig zu beachten, dass das Datumformat "2006-01-02" in Go eine besondere Bedeutung hat, da es sich um das Datum handelt, an dem die Go-Sprache offiziell veröffentlicht wurde. Daher wird es oft als Standardformat verwendet.

## Siehe auch

- Offizielle Dokumentation des "time" Pakets: [https://pkg.go.dev/time]
- Weitere Informationen zur Go-Sprache: [https://golang.org]
- Beispielprogramm zur Konvertierung von Daten in einen String: [https://play.golang.org/p/-FlTWHUvztP]