---
title:                "Die vergleichung von zwei Daten"
html_title:           "Go: Die vergleichung von zwei Daten"
simple_title:         "Die vergleichung von zwei Daten"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Vergleichen von zwei Datumsangaben ist eine häufige Aufgabe in der Programmierung. Es beinhaltet die Überprüfung, ob ein Datum kleiner, größer oder gleich einem anderen Datum ist. Programmierer verwenden dies, um z.B. zu überprüfen, ob eine bestimmte Frist abgelaufen ist oder um Daten in der richtigen Reihenfolge zu sortieren.

# Wie geht's:

```Go 
func compareDates(date1, date2 time.Time) int{
    if date1.Before(date2){
        return -1
    }else if date1.After(date2){
        return 1
    }else{
        return 0
    }
}

date1 := time.Date(2020, time.October, 01, 12, 00, 00, 00, time.UTC)
date2 := time.Date(2020, time.October, 10, 12, 00, 00, 00, time.UTC)

fmt.Println(compareDates(date1, date2)) // output: -1

```

Um zwei Datumsangaben in Go zu vergleichen, müssen wir zuerst die Zeitpakete importieren. Dann können wir die ```time.Time``` Struktur verwenden, um unsere Datumsangaben zu erstellen. Wir können dann die Funktion ```compareDates``` erstellen, die zwei Datumsangaben akzeptiert und eine negative Zahl zurückgibt, wenn das erste Datum kleiner ist, eine positive Zahl, wenn es größer ist, und 0, wenn sie gleich sind. In unserem Beispiel ist das Ergebnis -1, da das erste Datum früher als das zweite Datum ist.

# Tiefentauchen:

Das Vergleichen von zwei Datumsangaben hat einen wichtigen historischen Kontext, da es eng mit der Entwicklung von Kalendersystemen und Zeitrechnungen verbunden ist. Alternativen zu dieser Methode sind unter anderem die Verwendung von Unix-Zeitstempeln oder die Verwendung von speziellen Bibliotheken, die die Vergleiche erleichtern. Die Implementierung der Vergleichsfunktion in Go basiert auf dem Vergleich von Unix-Zeitstempeln, die die Anzahl der Sekunden seit dem 1. Januar 1970 darstellen.

# Siehe auch:

Weitere Informationen zu Datum und Zeit in Go finden Sie in der offiziellen Dokumentation unter https://golang.org/pkg/time/ und https://golang.org/pkg/time/#pkg-constants. Für Alternativen zur Verwendung von Unix-Zeitstempeln empfehlen wir die Pakete "timeutil" und "dateparse".