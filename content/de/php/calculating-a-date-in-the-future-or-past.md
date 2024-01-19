---
title:                "Eine zukünftige oder vergangene Datum berechnen"
html_title:           "PHP: Eine zukünftige oder vergangene Datum berechnen"
simple_title:         "Eine zukünftige oder vergangene Datum berechnen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Berechnung eines zukünftigen oder vergangenen Datums ist ein typisches Aufgabenfeld in zahlreichen Anwendungen, wie z. B. Planungstools, Kalender oder Reminder-Apps. Sie erlaubt die dynamische Generierung von Zeit- und Datumsinformationen basierend auf einem bestimmten Ausgangsdatum.

## So geht's:

In PHP können wir die integrierte Klasse `DateTime` und die Funktion `add` bzw. `sub` verwenden, um ein zukünftiges oder vergangenes Datum zu berechnen. Schauen wir uns ein Beispiel an:

```PHP
$dat = new DateTime('2022-01-01');
$intervall = new DateInterval('P1M2D'); // 1 Monat und 2 Tage Interval
$dat->add($intervall);
echo $dat->format('Y-m-d');
```

Ausgabe wird `2022-02-03` sein, d. h., ein Monat und zwei Tage nach dem 1. Januar 2022.

Für ein vergangenes Datum verwenden wir `sub`:

```PHP
$dat = new DateTime('2022-01-01');
$intervall = new DateInterval('P1M2D'); 
$dat->sub($intervall);
echo $dat->format('Y-m-d');
```

Ausgabe wird `2021-11-29` sein, d. h., ein Monat und zwei Tage vor dem 1. Januar 2022.

## Tiefere Details

Die PHP-Klasse `DateTime` wurde mit PHP 5.2 im Jahr 2006 eingeführt, um einen einheitlicheren und objektorientierten Ansatz zur Behandlung von Datums- und Zeitproblemen zu ermöglichen. Zuvor war die Behandlung von Daten und Zeiten in PHP eher ad hoc und weniger konsistent.

Es gibt diverse Alternativen zur `DateTime`-Klasse in PHP. Dazu gehören Funktionen wie `strtotime` und `mktime`. Obwohl sie zweckdienlich sind, bieten sie nicht den gleichen Grad an Flexibilität und Verständlichkeit wie die `DateTime`-Klasse.

Es ist wichtig zu beachten, dass die `DateTime::add`- und `DateTime::sub`-Methoden Änderungen direkt am `DateTime`-Objekt vornehmen. Wenn du das ursprüngliche Objekt unverändert lassen möchtest, dupliziere es zuerst mit der `clone`-Funktion.

## Weiterführende Informationen 

1. [Offizielle PHP-Dokumentation zur DateTime-Klasse](https://www.php.net/manual/de/class.datetime.php)
2. [Tutorial zur Date/Time-Manipulation in PHP](https://www.learn-php.org/en/Date_and_Time)
3. [Unterschied zwischen DateTime, strtotime und mktime](https://stackoverflow.com/questions/3896591/difference-between-datetime-strtotime-and-mktime)