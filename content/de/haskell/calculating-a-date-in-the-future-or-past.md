---
title:    "Haskell: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum
Das Berechnen von Daten in der Zukunft oder Vergangenheit kann für viele verschiedene Anwendungen nützlich sein, zum Beispiel für Kalender- oder Terminplanungsprogramme. In dieser Blog-Post werden wir uns ansehen, wie wir dies mithilfe von Haskell erreichen können.

## Wie Es Geht
Die Berechnung von Daten in der Zukunft oder Vergangenheit in Haskell kann mit Hilfe der `Day`- und `Time`-Module erfolgen. Schauen wir uns ein Beispiel an, in dem wir 10 Tage zur aktuellen Datum hinzufügen:

```Haskell
import Data.Time
import System.Locale

today = getCurrentTime
dateInTenDays = addDays 10 today
print dateInTenDays
```

Die Ausgabe dieses Codes wird das Datum in 10 Tagen sein. Wir können auch negative Werte mit `addDays` verwenden, um Daten in der Vergangenheit zu berechnen. Schauen wir uns nun an, wie wir auch die Uhrzeit berücksichtigen können:

```Haskell
import Data.Time
import System.Locale

today = getCurrentTime
timeInOneHour = addUTCTime (60 * 60) today
print timeInOneHour
```

Die Ausgabe wird das Datum und die Uhrzeit in einer Stunde sein. Beachten Sie, dass wir die Anzahl der Sekunden (60 * 60) multiplizieren, um die Anzahl der Sekunden in einer Stunde zu erhalten. Sie können diese Berechnung auch auf andere Zeiteinheiten anwenden, z.B. Minuten (60).

## Tief Einsteigen
In diesem Beispiel haben wir uns nur auf die Verwendung von `addDays` und `addUTCTime` konzentriert, aber es gibt noch viele weitere Funktionen, die für die Berechnung von Daten in der Zukunft oder Vergangenheit nützlich sein können. Zum Beispiel können Sie auch Monate, Jahre oder sogar bestimmte Tage der Woche mit `addMonths`, `addYears` und `addDaysOfWeek` hinzufügen. Es gibt auch Funktionen, mit denen Sie bestimmte Feiertage oder Schaltjahre berücksichtigen können. Indem Sie sich mit der Dokumentation des `Data.Time`-Moduls befassen, können Sie die perfekte Lösung für Ihre Anwendung finden.

## Siehe Auch
- [Offizielle Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Haskell Kurs auf Codecademy](https://www.codecademy.com/learn/learn-haskell)
- [ 20 Must-Read Haskell Blogs](https://dataeng.berlin/20-must-read-blogs-for-every-haskell-programmer)