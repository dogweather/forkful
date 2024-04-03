---
date: 2024-01-20 17:31:27.044460-07:00
description: "Wie geht das: Um ein Datum in der Zukunft oder Vergangenheit zu berechnen,\
  \ verwenden wir die `DateTime` und `DateInterval` Klassen. Hier ein paar\u2026"
lastmod: '2024-03-13T22:44:53.985802-06:00'
model: gpt-4-1106-preview
summary: Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, verwenden wir
  die `DateTime` und `DateInterval` Klassen.
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
weight: 26
---

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
