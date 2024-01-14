---
title:    "PHP: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann in der Programmierung sehr nützlich sein. Zum Beispiel kann es hilfreich sein, das Ablaufdatum eines Gutscheins oder die Lieferzeit eines Produktes zu bestimmen.

## Wie geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die PHP-Funktion `strtotime()` verwenden. Diese Funktion akzeptiert ein Datumsformat und gibt einen Timestamp zurück, der die Anzahl der Sekunden seit dem 1. Januar 1970 um 00:00 Uhr GMT darstellt. Mit diesem Timestamp können wir dann verschiedene Berechnungen durchführen.

Lass uns ein paar Beispiele anschauen:

**Berechnen eines Datums in der Zukunft:**

```PHP
$timestamp = strtotime("+3 days");
echo date("d.m.Y", $timestamp);
```
Ausgabe: 27.09.2021

**Berechnen eines Datums in der Vergangenheit:**

```PHP
$timestamp = strtotime("-1 month");
echo date("d.m.Y", $timestamp);
```
Ausgabe: 28.08.2021

Es ist auch möglich, ein bestimmtes Datum als Ausgangspunkt zu verwenden und dann eine Anzahl von Tagen, Monaten oder Jahren hinzuzufügen oder abzuziehen.

**Verwenden eines spezifischen Datums:**

```PHP
$timestamp = strtotime("10 September 2021");
echo date("d.m.Y", $timestamp);
```
Ausgabe: 10.09.2021

**Hinzufügen von 2 Monaten:**

```PHP
$timestamp = strtotime("+2 months", strtotime("5 October 2021"));
echo date("d.m.Y", $timestamp);
```
Ausgabe: 05.12.2021

Es gibt viele weitere Möglichkeiten, um mit der `strtotime()`-Funktion zu arbeiten. Es ist wichtig, das gewünschte Datumsformat zu beachten und eventuell vorhandene Zeit- und Zeitzonenangaben zu berücksichtigen.

## Tiefer gehen

Das Berechnen von Datumsangaben kann auch komplexer werden, besonders wenn es um die Berücksichtigung von Schaltjahren oder verschiedenen Kalenderformaten geht. In solchen Fällen kann es hilfreich sein, spezialisierte PHP-Funktionen wie `date_diff()` oder `DateTime` zu verwenden.

Es ist auch wichtig zu verstehen, dass Datumsangaben in der Programmierung oft in Form von Timestamps angegeben werden, die eine bestimmte Anzahl an Sekunden darstellen. Daher ist es wichtig, diese Timestamps korrekt zu interpretieren und bei der Berechnung und Umwandlung von Datumsangaben sorgfältig zu sein.

## Siehe auch

- [PHP Dokumentation: strtotime()](https://www.php.net/manual/de/function.strtotime.php)
- [PHP Dokumentation: date_diff()](https://www.php.net/manual/de/datetime.diff.php)
- [PHP Dokumentation: DateTime](https://www.php.net/manual/de/datetime.settimestamp.php)