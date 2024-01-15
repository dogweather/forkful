---
title:                "Vergleich von zwei Daten"
html_title:           "Go: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleichen von zwei Daten ist eine häufige Aufgabe in der Programmierung, vor allem wenn man mit Datumsfunktionen arbeitet. Es ist wichtig zu wissen, wie man diese Vergleiche richtig durchführt, um zuverlässige Ergebnisse und eine stabile Anwendung zu gewährleisten.

## So geht's

Um zwei Daten in Go zu vergleichen, können wir die Funktion "Equal" aus der "time" Standardbibliothek verwenden. Diese Funktion akzeptiert zwei Parameter vom Typ "Time" und gibt eine boolesche Wert zurück, der angibt, ob die beiden Zeiten gleich sind oder nicht.

```Go
import "fmt"
import "time"

func main() {
    // Erstes Datum erstellen
    t1 := time.Date(2020, time.September, 1, 0, 0, 0, 0, time.UTC)
    // Zweites Datum erstellen
    t2 := time.Date(2020, time.December, 1, 0, 0, 0, 0, time.UTC)
    // Vergleich durchführen
    if t1.Equal(t2) {
        fmt.Println("Die beiden Daten sind gleich!")
    } else {
        fmt.Println("Die beiden Daten sind nicht gleich!")
    }
}
```

Output:

```
Die beiden Daten sind nicht gleich!
```

Dieses Beispiel zeigt, wie wir zwei Daten miteinander vergleichen können. Um den Vergleich noch genauer zu gestalten, können wir auch die Funktionen "Before" und "After" verwenden, welche die relative Position der beiden Daten zueinander bestimmt.

```Go
// t1 ist ein früheres Datum als t2
if t1.Before(t2) {
    fmt.Printf("%v liegt vor %v\n", t1, t2)
}
// t2 ist ein späteres Datum als t1
if t2.After(t1) {
    fmt.Printf("%v liegt nach %v\n", t2, t1)
}
```

Output:

```
2020-09-01 00:00:00 +0000 UTC liegt vor 2020-12-01 00:00:00 +0000 UTC
```

## Tief eintauchen

Beim Vergleichen von zwei Daten in Go gibt es ein paar wichtige Dinge zu beachten. Zum einen sollten die beiden Daten vom selben Typ sein, also beide vom Typ "Time". Andernfalls müssen wir sie mit der Funktion "Parse" in dieses Format umwandeln. Außerdem sollten wir immer sicherstellen, dass die Zeitzone der beiden Daten übereinstimmt, da dies Auswirkungen auf das Ergebnis haben kann.

Eine andere wichtige Sache ist, dass wir beim Vergleich von zwei Daten nicht einfach die String-Repräsentation vergleichen können. Stattdessen müssen wir immer die "Equal" Funktion verwenden, um zuverlässige Ergebnisse zu erhalten.

Schließlich ist es auch möglich, zwei Daten mithilfe von "unix time" zu vergleichen. Hierbei wird die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 verwendet. In Go können wir dies mit der Funktion "Unix" erreichen.

## Siehe auch

- [Offizielle Dokumentation zur "time" Standardbibliothek](https://pkg.go.dev/time)
- [Tutorial: Datum und Uhrzeit in Go](https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-go-de)
- [Unix time im Detail erklärt](https://www.epochconverter.com/)