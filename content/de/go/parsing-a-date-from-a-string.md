---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist der Prozess, einen Textstring anzunehmen, der ein Datum repräsentiert, und diesen in ein für die Software verständliches Format zu konvertieren. Programmierer machen das, um mit solchen Daten zu arbeiten, Berechnungen durchzuführen oder Datumsangaben zu speichern und zu manipulieren.

## So geht's:

Mit Go können wir Datumswerte mit der Funktion `time.Parse` parsen. Hier ist ein einfaches Beispiel:

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  str := "2022-07-18"
  layout := "2006-01-02"
  t, _ := time.Parse(layout, str)
  fmt.Println(t)
}
```

Dieser Code gibt folgendes aus:

```Shell
2022-07-18 00:00:00 +0000 UTC
```

## Vertiefung

Historischer Kontext: Die Interpretation von Datumswerten aus Strings kann auf den Beginn der Informatik zurückverfolgt werden. Heutzutage gibt es viele Möglichkeiten, Datumsinformationen in Textformat zu speichern, und dementsprechend viele Methoden, diese Informationen zu parsen.

Alternativen: Go bietet auch andere Funktionen wie `time.ParseInLayout`, die es ermöglichen, das Eingabeformat besser zu steuern. Es ist wichtig, die richtige Methode zu wählen, die den Anforderungen jeder Situation entspricht.

Implementierungsdetails: Unter der Haube verwendet `time.Parse` eine Layout-Zeichenkette, um das erwartete Format des Eingabestrings festzulegen. Diese Layout-Zeichenkette muss das Referenzdatum 2006-01-02 15:04:05 sein, das dann anhand der gegebenen Zeichenkette modifiziert wird.

## Weiterführende Informationen

1. Die Go Standardbibliothek Dokumentation für das `time` Paket: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
2. Ein ausführlicher Artikel über das Arbeiten mit Datums- und Zeitwerten in Go: [https://yourbasic.org/golang/format-parse-string-time-date-example/](https://yourbasic.org/golang/format-parse-string-time-date-example/)
3. Ein Beispiel für das Parsen von Datums- und Zeitstempeln aus einem JSON-Feed: [https://www.alexedwards.net/blog/formatting-date-time-and-number-values-in-go](https://www.alexedwards.net/blog/formatting-date-time-and-number-values-in-go)