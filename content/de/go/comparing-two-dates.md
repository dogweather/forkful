---
title:    "Go: Vergleich von zwei Daten"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist eine häufige Aufgabe, die in vielen Programmiersprachen, einschließlich Go, erforderlich ist. Es ermöglicht uns, festzustellen, ob ein Datum vor, nach oder gleich einem anderen Datum liegt. In diesem Blog-Beitrag werden wir uns daher ansehen, wie wir dies in Go effizient und korrekt tun können.

## Wie Vergleiche ich zwei Daten in Go

Um zwei Daten in Go zu vergleichen, können wir die "Before", "After" oder "Equal" Methoden der Zeitstruktur verwenden. Diese Methode gibt ein boolesches Ergebnis zurück und ermöglicht uns somit, die Vergleichsergebnisse in einer Bedingungsanweisung zu verwenden.

Lass uns ein Beispiel schauen:

```Go
now := time.Now() // Aktuelles Datum und Uhrzeit
later := now.Add(time.Hour * 24) // Datum und Uhrzeit in 24 Stunden

// Vergleich der beiden Daten
if now.Before(later) {
    fmt.Printf("%v liegt vor %v", now, later)
} else if now.After(later) {
    fmt.Printf("%v liegt nach %v", now, later)
} else {
    fmt.Printf("%v und %v sind gleich", now, later)
}

```

In diesem Beispiel verwenden wir die Methoden "Before" und "After" und geben eine entsprechende Ausgabe abhängig davon, ob die Daten vor oder nach einander liegen. Alternativ könnten wir auch die "Equal" Methode verwenden, um festzustellen, ob die Daten gleich sind.

## Tiefer Einblick

Beim Vergleichen von Daten in Go gibt es einige wichtige Aspekte zu beachten. Zum einen berücksichtigt Go den Zeitpunkt der Erstellung des Zeitobjekts in der Vergleichsmethode. Das bedeutet, dass selbst wenn zwei Zeitobjekte die gleiche Uhrzeit darstellen, sie aufgrund ihres Erstellungszeitpunkts möglicherweise nicht als gleich erachtet werden.

Zum anderen können wir auch bestimmte Zeitformate verwenden, um die Genauigkeit des Vergleichs zu steuern. Zum Beispiel könnten wir nur das Datum ohne Zeitangabe berücksichtigen oder auch die Millisekunden miteinbeziehen.

Es ist auch wichtig zu beachten, dass bei Verwendung der "Equal" Methode die Zeitzone des Zeitobjekts berücksichtigt wird. In manchen Fällen kann es daher sinnvoll sein, vor allem bei internationalen Anwendungen, die Daten in der UTC (Coordinated Universal Time) zu speichern und zu vergleichen.

## Siehe auch

- Offizielle Go-Dokumentation zu Zeit und Datum: [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
- Tutorial: Zeiten und Datum in Go: [https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-go-de](https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-go-de)
- Zeitzone in Go verwalten: [https://blog.gopheracademy.com/advent-2016/go-time-zones/](https://blog.gopheracademy.com/advent-2016/go-time-zones/)