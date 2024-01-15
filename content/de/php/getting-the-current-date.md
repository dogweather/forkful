---
title:                "Das aktuelle Datum erhalten"
html_title:           "PHP: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist eine grundlegende Funktion in der PHP-Programmierung. Es ermöglicht Entwicklern, dynamische Funktionen basierend auf dem aktuellen Datum zu erstellen, wie beispielsweise einen Kalender oder eine Countdown-Funktion.

## Wie

Die aktuelle Zeit und das Datum können mit der `date()`-Funktion in PHP abgerufen werden. Diese Funktion benötigt zwei Argumente: ein Format und eine Zeitangabe.

Beispielcode:

```PHP
<?php
// Aktuelles Datum im Format "Tag/Monat/Jahr"
echo date("d/m/Y"); 

// Aktuelle Uhrzeit im Format "Stunde:Minute:Sekunde"
echo date("H:i:s"); 
?>
```

Ausgabe:

```
13/08/2020
13:45:21
```

Die Formatierung des Datums kann je nach Bedarf angepasst werden. Eine vollständige Liste der verfügbaren Formatierungszeichen finden Sie in der offiziellen [PHP-Dokumentation] (https://www.php.net/manual/en/function.date.php).

## Deep Dive

Die `date()`-Funktion basiert auf der Systemzeit des Servers, auf dem die PHP-Anwendung läuft. Daher kann das Datum in verschiedenen Zeitzonen variieren. Um die Anwendung von Zeitzonen zu berücksichtigen, kann die `date_default_timezone_set()`-Funktion verwendet werden.

Beispielcode:

```PHP
<?php
// Setzen der Zeitzone auf Berlin
date_default_timezone_set("Europe/Berlin");

// Aktuelles Datum und Uhrzeit im Format "Tag/Monat/Jahr, Stunde:Minute:Sekunde"
echo date("d/m/Y, H:i:s");
?>
```

Ausgabe:

```
13/08/2020, 14:00:12
```

Es ist auch möglich, das aktuelle Datum in einem bestimmten Format in einem anderen Land abzurufen, indem die entsprechende Zeitzone festgelegt wird. Eine Liste der verfügbaren Zeitzone können Sie in der [PHP-Dokumentation] (https://www.php.net/manual/en/timezones.php) finden.

## Siehe auch

- [PHP date() Funktion] (https://www.php.net/manual/en/function.date.php)
- [PHP Zeitzone festlegen] (https://www.php.net/manual/en/function.date-default-timezone-set.php)