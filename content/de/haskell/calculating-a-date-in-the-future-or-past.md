---
title:                "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Haskell: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum

Manchmal möchte man ein Datum in der Zukunft oder Vergangenheit berechnen, zum Beispiel für Aufgabenplanung oder um Alter zu berechnen. Mit Haskell ist es möglich, solche Berechnungen einfach und präzise durchzuführen.

# Wie geht das?

Das Berechnen von Datumsangaben in Haskell ist dank der `Data.Time` Bibliothek ganz einfach. Zuerst müssen wir diese importieren:

```Haskell
import Data.Time
```

Um ein bestimmtes Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die `addDays` Funktion verwenden. Diese Funktion nimmt eine Anzahl von Tagen und ein `Day`-Objekt als Parameter und gibt ein neues `Day`-Objekt zurück, das um die angegebene Anzahl von Tagen verschoben wurde. Zum Beispiel können wir das Datum von heute in 10 Tagen berechnen:

```Haskell
addDays 10 today
```

Die `today` Funktion gibt das aktuelle Datum als `Day`-Objekt zurück, welches wir als Parameter für `addDays` verwenden können.

Die Ausgabe dieses Codes ist ein neues `Day`-Objekt, das 10 Tage in der Zukunft liegt. Wir können dieses Objekt dann weiterverwenden, um beispielsweise den Wochentag des berechneten Datums zu ermitteln:

```Haskell
let zukunftsDatum = addDays 10 today
let wochentag = dayOfWeek zukunftsDatum
```

In diesem Beispiel wird der Wochentag des Datums berechnet und in der Variablen `wochentag` gespeichert.

# Tiefer Einblick

Die `Data.Time` Bibliothek bietet viele weitere Funktionen, die für die Berechnung von Datumsangaben nützlich sein können. Mit der `addGregorianMonthsClip` Funktion können wir beispielsweise Monate anstatt von Tagen hinzufügen, was besonders hilfreich ist, wenn wir aus einem Datum eines bestimmten Monats das Datum des gleichen Tages im nächsten Monat berechnen möchten.

Es gibt auch Funktionen zum Berechnen von Datumsgrenzen, wie zum Beispiel `endOfMonth` oder `beginningOfYear`, die jeweils das letzte bzw. erste Datum eines bestimmten Monats oder Jahres berechnen.

Eine vollständige Liste aller Funktionen der `Data.Time` Bibliothek und weitere Dokumentation finden Sie auf der offiziellen Haskell-Website.

# Siehe auch

- Die offizielle Haskell-Website: https://www.haskell.org/
- Dokumentation zur `Data.Time` Bibliothek: https://hackage.haskell.org/package/time/docs/Data-Time.html