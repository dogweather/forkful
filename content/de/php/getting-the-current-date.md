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

# Aktuelles Datum in PHP 
 
## Was & Warum?
Die aktuelle Datum Funktion in PHP ermöglicht es Programmierern, das Datum und die Uhrzeit in einem bestimmten Format auszugeben. Sie ist hilfreich für Anwendungen, die das Datum und die Uhrzeit für verschiedene Zwecke benötigen, wie z.B. für ein Log-in-System oder für die Anzeige von Live-Informationen auf einer Webseite.

## Wie:
Der Code ```PHP echo date('d.m.Y H:i:s'); ``` gibt das aktuelle Datum und die Uhrzeit in dem Format "Tag.Monat.Jahr Stunde:Minute:Sekunde" aus. Das Ergebnis könnte zum Beispiel wie folgt aussehen: "01.04.2021 12:30:15". Das Format kann nach Belieben angepasst werden, indem man die Parameter in der ``` date() ``` Funktion ändert.

## Tiefere Einblicke:
Die PHP Funktion zur Ausgabe des aktuellen Datums gibt es seit der Version 3.0 des Programmierens PHP. Vorher mussten Programmierer auf externe Bibliotheken zurückgreifen. Alternativ zur Verwendung der ``` date() ``` Funktion, gibt es auch die Möglichkeit die Bibliothek "DateTime" zu importieren und diese zu verwenden. Dies ermöglicht weitere Funktionen, wie z.B. das Rechnen mit Datum und Uhrzeit.

## Siehe auch:
- [PHP-Manual: Date()](https://www.php.net/manual/en/function.date.php)
- [PHP-Manual: DateTime](https://www.php.net/manual/en/class.datetime.php)