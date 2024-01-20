---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Go Programmierung: Vergleich von zwei Daten

## Was & Warum?

Das Vergleichen von zwei Daten bedeutet, zu bestimmen, ob ein Datum vor, nach oder gleich einem anderen Datum ist. Entwickler tun das oft, um Zeitabläufe zu handhaben und logische Bedingungen basierend auf Zeitstempeln einzurichten.

## Wie geht das?

In Go, datumsvergleiche können einfach und intuitiv durchgeführt werden. Hier ist ein Beispielcode:

```Go
package main
import (
  "fmt"
  "time"
)

func main() {
  date1 := time.Date(2021, 1, 1, 0, 0, 0, 0, time.UTC)
  date2 := time.Date(2022, 1, 1, 0, 0, 0, 0, time.UTC)
  
  if date1.Before(date2) {
    fmt.Println("Datum 1 ist vor Datum 2")
  } else if date1.After(date2) {
    fmt.Println("Datum 1 ist nach Datum 2")
  } else if date1.Equal(date2) {
    fmt.Println("Die Daten sind gleich")
  }
}
```

Beim Ausführen dieses Codes wird "Datum 1 ist vor Datum 2" angezeigt.

## Tiefere Einblicke

Historisch gesehen gab es in den früheren Versionen von Go keine eingebauten Funktionen für den Direktvergleich von zwei Daten. Entwickler mussten die einzelnen Zeitkomponenten extrahieren und vergleichen, was zu mehr Code und möglichen Fehlern führen konnte. 

Alternativ kann man die `Sub`-Funktion von Go verwenden, um die Differenz zwischen zwei Daten zu berechnen. Allerdings ist das nur nützlich, wenn man wissen will, wie viel Zeit zwischen den Daten vergangen ist, weniger wenn man nur wissen möchte, welches Datum früher oder später ist.

Die Vergleichsfunktionen `Before`, `After` und `Equal` vergleichen direkt die Unix-Zeitstempel der Daten. Das sind lange Zahlen, die die Sekunden darstellen, die seit dem 1. Januar 1970 vergangen sind (auch bekannt als die "Unix-Ära").

## Siehe Auch

Weitere nützliche Ressourcen können folgende sein:

- Die offizielle Go-Dokumentation zur Zeitbibliothek: https://golang.org/pkg/time/
- Ein Tutorial über das Arbeiten mit Daten in Go: https://yourbasic.org/golang/format-parse-string-time-date-example/
- Go Zeitvergleich auf StackOverflow: https://stackoverflow.com/questions/22579434/compare-date-part-only-without-time-part-in-them-in-go