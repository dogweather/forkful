---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
aliases:
- de/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:27.044460-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Berechnungen von zukünftigen oder vergangenen Daten ermöglichen es, Zeitintervalle zu bestimmen. Programmierer nutzen das, um Deadline-Management, Erinnerungsfunktionen oder Terminverfolgungen in Anwendungen zu implementieren.

## Wie geht das:
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, verwenden wir die `DateTime` und `DateInterval` Klassen. Hier ein paar Beispiele:

```php
<?php
$heute = new DateTime(); // heute
$interval = new DateInterval('P10D'); // 10 Tage

$zukunft = clone $heute;
$zukunft->add($interval); // Datum in der Zukunft
echo $zukunft->format('Y-m-d') . "\n"; // Beispiel-Ausgabe: 2023-04-21

$vergangenheit = clone $heute;
$vergangenheit->sub($interval); // Datum in der Vergangenheit
echo $vergangenheit->format('Y-m-d') . "\n"; // Beispiel-Ausgabe: 2023-04-01
?>
```

## Deep Dive
Die Berechnung von Datumsangaben hat historische Wurzeln in Kalendersystemen und Zeitmessung. PHP bietet mehrere Wege, um mit Daten zu arbeiten. Neben `DateTime` und `DateInterval` gibt es auch Funktionen wie `strtotime`, die mit Strings arbeitet:

```php
$zukunft = date('Y-m-d', strtotime('+10 days'));
$vergangenheit = date('Y-m-d', strtotime('-10 days'));
```

Die `DateTime` Klasse wurde in PHP 5.2.0 eingeführt und wird aufgrund besserer OOP-Praktiken und Fehlerbehandlung empfohlen. Funktionen wie `strtotime()` funktionieren zwar auch, aber sie sind weniger flexibel und fehleranfällig bei komplexen Berechnungen.

## Siehe auch:
- PHP Manual on DateTime: https://www.php.net/manual/de/class.datetime.php
- PHP Manual on DateInterval: https://www.php.net/manual/de/class.dateinterval.php
- PHP Manual on strtotime: https://www.php.net/manual/de/function.strtotime.php
