---
title:    "Gleam: Vergleich von zwei Daten"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum
In der Programmierung ist es oft notwendig, verschiedene Daten zu vergleichen. Das gilt auch für Datumsangaben. Mit Gleam ist es möglich, auf einfache Art und Weise zwei Datumsangaben zu vergleichen.

## Wie geht man vor?
Um zwei Datumsangaben in Gleam zu vergleichen, benötigt man die Funktion `Datetime.diff`. Diese Funktion nimmt zwei Argumente entgegen und gibt das Ergebnis des Vergleichs in Sekunden zurück.

```Gleam
let start_date = Datetime.local(2021, 4, 20, 12, 00)
let end_date = Datetime.local(2021, 5, 1, 18, 00)

let difference = Datetime.diff(start_date, end_date)

IO.println("Die Differenz beträgt: ${difference} Sekunden")
```

Die Ausgabe dieses Beispiels würde lauten: `Die Differenz beträgt: 950400 Sekunden`. Dies entspricht 11 Tagen, 6 Stunden und 0 Minuten.

## Tief in die Materie eintauchen
Es ist auch möglich, verschiedene Einheiten für die Differenz anzugeben, indem man ein drittes Argument an `Datetime.diff` übergibt. Mögliche Einheiten sind `Datetime.Year`, `Datetime.Month`, `Datetime.Day`, `Datetime.Hour`und `Datetime.Minute`.

```Gleam
let start_date = Datetime.local(2021, 4, 20, 12, 00)
let end_date = Datetime.local(2021, 5, 1, 18, 00)

let difference = Datetime.diff(start_date, end_date, Datetime.Day)

IO.println("Die Differenz beträgt: ${difference} Tage")
```

Die Ausgabe dieses Beispiels würde lauten: `Die Differenz beträgt: 11 Tage`.

## Siehe auch
- [Offizielle Dokumentation zu Datumsangaben in Gleam](https://gleam.run/core/datetime.html)
- [Weitere nützliche Funktionen für Datumsangaben in Gleam](https://gleam.run/library/datetime.html)