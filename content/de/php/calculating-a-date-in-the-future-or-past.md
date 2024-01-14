---
title:    "PHP: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Warum
In dieser Blog-Post geht es darum, wie man mit PHP ein Datum in der Zukunft oder Vergangenheit berechnen kann. Das kann besonders nützlich sein, wenn man z.B. eine Booking- oder Reservierungsfunktion für eine Website erstellt und das Datum der Reservierung im Voraus anzeigen möchte.

## Wie geht das?

Die Berechnung eines Datums in der Zukunft oder Vergangenheit kann mit einer Kombination aus der `strtotime()` und `date()` Funktion in PHP durchgeführt werden.

#### Beispiel 1: Datum in der Zukunft berechnen
```PHP
$future_date = strtotime("+1 week"); // Adds one week to the current date
echo date("d.m.Y", $future_date); // Outputs the calculated date in the format "day.month.year"
```
Output: 21.08.2020

#### Beispiel 2: Datum in der Vergangenheit berechnen
```PHP
$past_date = strtotime("-3 days"); // Subtracts three days from the current date
echo date("d.m.Y", $past_date); // Outputs the calculated date in the format "day.month.year"
```
Output: 12.08.2020

## Tiefergehende Infos
Die `strtotime()` Funktion konvertiert einen gegebenen String in einen Unix-Zeitstempel, welcher dann von der `date()` Funktion in ein lesbares Datum umgewandelt wird. Die Funktion akzeptiert verschiedene Parameter, wie z.B. "next Monday", "+2 weeks", "last day of next month" usw. Eine vollständige Liste der Parameter und deren Ausgabe ist in der [offiziellen PHP-Dokumentation](https://www.php.net/manual/de/function.strtotime.php) zu finden.

Es ist auch möglich, ein spezifisches Datum als Basis anzugeben, anstatt vom aktuellen Datum aus zu rechnen. Dafür kann man das zweite Argument bei der `strtotime()` Funktion verwenden.

#### Beispiel 3: Berechnung vom 1. Januar 2021 aus
```PHP
$base_date = strtotime("01/01/2021"); // Sets the 1st of January 2021 as base date
$calculated_date = strtotime("+2 months", $base_date); // Adds two months to the base date
echo date("d.m.Y", $calculated_date); // Outputs the calculated date in the format "day.month.year"
```
Output: 01.03.2021

## Siehe auch
- [Offizielle PHP-Dokumentation zu strtotime()](https://www.php.net/manual/de/function.strtotime.php)
- [Offizielle PHP-Dokumentation zu date()](https://www.php.net/manual/de/function.date.php)
- [Tutorial: Wie man mit PHP Datum und Zeit berechnet](https://www.php-einfach.de/experte/php-tutorial/datum-zeit-berechnen/)