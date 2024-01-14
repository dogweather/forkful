---
title:                "PHP: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum

Das Berechnen von zukünftigen oder vergangenen Daten kann hilfreich sein, um beispielsweise Geburtstage, Feiertage oder Vertragslaufzeiten im Voraus zu planen.

# Wie geht's

Die Berechnung von zukünftigen oder vergangenen Daten kann in PHP mithilfe der Funktionen "strtotime" und "date" durchgeführt werden. Diese Funktionen ermöglichen es uns, basierend auf einem gegebenen Datum und einer gewünschten Anzahl von Tagen, Wochen, Monaten oder Jahren, das entsprechende Datum zu berechnen.

```PHP
$today = date("Y-m-d"); // aktuelles Datum, z.B. 2021-09-17
// Berechnung des Datums in 45 Tagen
$future_date = date("Y-m-d", strtotime("+45 days", $today)); // 2021-11-01
// Berechnung des Datums vor 1 Monat
$past_date = date("Y-m-d", strtotime("-1 month", $today)); // 2021-08-17
```

In diesem Beispiel wird das heutige Datum verwendet, um zukünftige und vergangene Daten zu berechnen. Sie können jedoch jedes andere Datum als Ausgangspunkt verwenden.

# Tiefergehende Informationen

Die in diesem Beispiel verwendeten Funktionen "strtotime" und "date" sind Teil der Date und Time Funktionen in PHP. Sie können auch mit anderen Parametern wie bestimmten Wochentagen, Feiertagen oder Zeitzonen verwendet werden. Es gibt auch andere Funktionen, wie zum Beispiel "mktime", die es ermöglichen, ein spezifisches Datum basierend auf einzelnen Werten für Jahr, Monat und Tag zu erstellen.

Weitere Informationen und Beispiele zu Datum und Zeitberechnungen in PHP finden Sie in der offiziellen Dokumentation unter: https://www.php.net/datetime

# Siehe auch

- https://www.php.net/datetime
- https://www.php.net/manual/de/function.strtotime.php
- https://www.php.net/manual/de/function.date.php
- https://www.php.net/manual/de/function.mktime.php