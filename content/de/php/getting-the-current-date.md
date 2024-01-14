---
title:                "PHP: Das aktuelle Datum erhalten"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine grundlegende Funktion in der PHP-Programmierung, die in vielen Anwendungsfällen benötigt wird. Es ermöglicht Entwicklern, die aktuelle Zeit und das Datum in ihren Skripten aufzurufen und sie für Berechnungen, Scheduling oder Datumsangaben zu verwenden. In diesem Blogbeitrag werden wir uns anschauen, wie man das aktuelle Datum in PHP abrufen kann.

## Wie geht man vor?

Das Abrufen des aktuellen Datums in PHP ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen wir die Funktion "date()" verwenden, die das aktuelle Datum und die Zeit als String zurückgibt. Die Syntax lautet: 

```PHP
$date = date("d.m.Y");
```

Dieser Code gibt uns das aktuelle Datum im Format "Tag.Monat.Jahr" zurück. Wir können aber auch das Format anpassen, indem wir verschiedene Parameter in der Funktion verwenden. Zum Beispiel:

```PHP
$date = date("F d, Y");
```

Dieser Code gibt uns das aktuelle Datum im Format "Monat Tag, Jahr" zurück. Es gibt viele verschiedene Parameter, die in der date()-Funktion verwendet werden können, um das Format des Datums und der Zeit anzupassen. Eine vollständige Liste ist in der offiziellen PHP-Dokumentation verfügbar.

## Deep Dive

Die date()-Funktion verwendet den Unix-Timestamp, um das aktuelle Datum und die Zeit zu berechnen. Der Unix-Timestamp ist eine Zahl, die die Anzahl der vergangenen Sekunden seit dem 1. Januar 1970 um 00:00 Uhr UTC darstellt. Durch die Verwendung dieser Zahl kann die date()-Funktion das Datum und die Zeit in beliebigen Zeitzonen berechnen, indem sie die lokale Zeitzone des Servers oder die in der PHP-Konfiguration festgelegte Zeitzone verwendet. 

Um das Datum und die Zeit in einer anderen Zeitzone abzurufen, müssen wir den zweiten Parameter der date()-Funktion verwenden, der die Zeitzone angibt. Zum Beispiel:

```PHP
$date = date("d.m.Y H:i:s", time(), "Europe/Berlin");
```

Dieser Code gibt uns das aktuelle Datum und die Zeit in Berlin zurück. Eine Liste aller verfügbaren Zeitzonen finden Sie in der offiziellen PHP-Dokumentation.

## Siehe auch

- Offizielle PHP-Dokumentation zu date(): https://www.php.net/manual/de/function.date.php
- Liste aller Zeitzonen in PHP: https://www.php.net/manual/de/timezones.php
- Weitere Tipps und Tricks zur Arbeit mit Datum und Zeit in PHP: https://www.php.net/manual/de/book.datetime.php