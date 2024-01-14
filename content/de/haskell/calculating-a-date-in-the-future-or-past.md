---
title:                "Haskell: Eine Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Eine Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals ein Programm geschrieben hast, das auf Datum oder Uhrzeit basiert, hast du wahrscheinlich schon einmal die Herausforderung gehabt, ein Datum in der Zukunft oder Vergangenheit zu berechnen. In diesem Blogbeitrag werden wir uns genau das anschauen - wie man Daten in der Zukunft oder Vergangenheit berechnet.

## How To

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die ```addDays``` Funktion aus der ```Data.Time.Calendar``` Bibliothek verwenden. Diese Funktion nimmt zwei Parameter - eine Anzahl von Tagen und ein Datum - und gibt ein neues Datum zurück, das die angegebene Anzahl von Tagen in der Zukunft oder Vergangenheit liegt.

Hier ist ein Beispiel, um 10 Tage in der Zukunft zu berechnen:

```Haskell
import Data.Time.Calendar

zukunft = addDays 10 $ fromGregorian 2021 5 1
```

Dieses Beispiel verwendet das aktuelle Datum als Basis und gibt ein Datum in zehn Tagen zurück. Um eine Vergangenheit einzurechnen, können wir einfach eine negative Anzahl von Tagen verwenden.

```Haskell
import Data.Time.Calendar

vergangenheit = addDays (-5) $ fromGregorian 2021 5 1
```

Dieses Beispiel würde ein Datum vor fünf Tagen zurückgeben.

## Deep Dive

Wenn wir tiefer in das Konzept der Datumsberechnung eintauchen, werden wir feststellen, dass es auch Funktionen gibt, um die Jahre, Monate oder sogar Stunden zu berechnen. Zum Beispiel können wir die ```addMonths``` Funktion verwenden, um eine bestimmte Anzahl von Monaten zu berechnen. Diese Funktion berücksichtigt auch das Schaltjahr und die verschiedenen Anzahl von Tagen in Monaten.

```Haskell
import Data.Time.Calendar

zukunft = addMonths 2 $ fromGregorian 2021 5 1
```

Dieses Beispiel würde das Datum in zwei Monaten zurückgeben, berücksichtigt aber Schaltjahre und die Anzahl von Tagen in jedem Monat.

## Siehe auch

- [Haskell-Dokumentation zu Data.Time.Calendar](http://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
- [Einführung in die Zeitberechnung in Haskell](https://wiki.haskell.org/Time)
- [Berechnungen mit Datum und Uhrzeit in Haskell](https://guide.aelve.com/haskell/time)