---
title:    "Go: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datum in der Zukunft oder Vergangenheit kann für viele Programmieraufgaben nützlich sein, wie z.B. das Planen von Terminen, das Generieren von Erinnerungen oder das Verwalten von Abonnements. In diesem Blogbeitrag werden wir uns ansehen, wie man dies in der Programmiersprache Go durchführt.

## Wie geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Funktion `AddDate()` verwenden. Diese Funktion nimmt drei Argumente entgegen: Jahr, Monat und Tag. Wir können entweder positiv oder negativ Zahlen für das Jahr, den Monat und den Tag angeben, je nachdem ob wir das Datum in der Zukunft oder Vergangenheit berechnen möchten.

Hier ist ein Beispiel, um das Datum von heute aus in zwei Wochen zu berechnen:

```Go
heute := time.Now()
zukunft := heute.AddDate(0, 0, 14)
```

In diesem Beispiel verwenden wir `AddDate()` um 0 Jahre, 0 Monate und 14 Tage zu unserem aktuellen Datum hinzuzufügen. Das Ergebnis, `zukunft`, wird das Datum in zwei Wochen von heute sein.

Lassen Sie uns nun ein Beispiel betrachten, um das Datum von heute aus vor zwei Jahren zu berechnen:

```Go
heute := time.Now()
vergangenheit := heute.AddDate(-2, 0, 0)
```

In diesem Beispiel verwenden wir `AddDate()` um 2 Jahre von unserem aktuellen Datum abzuziehen. Das Ergebnis, `vergangenheit`, wird das Datum vor zwei Jahren von heute sein.

## Tiefergehende Informationen

Die Funktion `AddDate()` kann auch für komplexere Berechnungen verwendet werden, wie z.B. das Berechnen von Datum unter Berücksichtigung von Schaltjahren. Außerdem akzeptiert sie zusätzliche Argumente wie Stunden, Minuten und Sekunden für eine genauere Datumsberechnung.

Um mehr über die Verwendung von `AddDate()` in verschiedenen Szenarien zu erfahren, empfehle ich Ihnen, die offizielle Dokumentation von Go zu lesen.

## Siehe auch

- Offizielle Dokumentation zu `AddDate()`: https://golang.org/pkg/time/#Time.AddDate
- Beispielcode für die Verwendung von `AddDate()`: https://play.golang.org/p/jv0do6ZYG0G