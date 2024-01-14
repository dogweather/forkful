---
title:    "Gleam: Das Berechnen eines Datums in der Zukunft oder Vergangenheit."
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von zukünftigen oder vergangenen Daten kann in vielen Situationen nützlich sein, sei es für die Planung von Veranstaltungen oder die Verwaltung von Fristen. Die Gleam-Programmiersprache bietet eine einfache Möglichkeit, diese Berechnungen durchzuführen.

## How To

Die Berechnung von zukünftigen oder vergangenen Daten in Gleam kann mit Hilfe des `DateTime`-Moduls erfolgen. Zunächst müssen wir das Modul importieren, indem wir `DateTime` am Anfang unseres Codes angeben:

```Gleam
import DateTime
```

Anschließend können wir eine Instanz der Struktur `DateTime` erstellen, indem wir den aktuellen Zeitstempel und die Anzahl der gewünschten Tage als Argumente übergeben. Zum Beispiel:

```Gleam
let future_date = DateTime.now().add_days(5)
let past_date = DateTime.now().sub_days(10)
```

Die `add_days`-Methode fügt der aktuellen Zeit das angegebene Datum hinzu, während die `sub_days`-Methode es von der aktuellen Zeit abzieht. Wir können auch andere Zeiteinheiten wie Stunden, Minuten oder sogar Monate hinzufügen oder abziehen.

Um das Ergebnis zu überprüfen, können wir die `format`-Methode verwenden, um das Datum in einem bestimmten Format auszugeben. Zum Beispiel:

```Gleam
let formatted_date = DateTime.format(past_date, "{dd}.{mm}.{yyyy}")
```

Dies würde das Datum im Format "TT.MM.JJJJ" ausgeben.

## Deep Dive

Das `DateTime`-Modul bietet auch weitere nützliche Funktionen für die Arbeit mit Datumsangaben, wie zum Beispiel die Berechnung der Differenz zwischen zwei Daten oder das Vergleichen von Daten. Dazu ist es wichtig, dass die Daten in der richtigen Zeitzone angegeben sind. Die Gleam-Dokumentation bietet weitere Informationen und Beispiele zur Verwendung des `DateTime`-Moduls.

## Siehe auch

- [Gleam-Dokumentation zum DateTime-Modul](https://gleam.run/modules/datetime)
- [Tutorial zur Verwendung von Gleam-Datumsangaben](https://gleam.run/docs/tutorials/dates)