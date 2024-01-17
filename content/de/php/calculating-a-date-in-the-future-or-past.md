---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "PHP: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines Datums in der Zukunft oder Vergangenheit ist ein häufiger Programmierbedarf, besonders in Anwendungen wie Kalendern oder Terminplanern. Es ermöglicht uns, Datumsangaben zu manipulieren und zu verarbeiten, um dynamische Funktionen zu erstellen.

## Wie geht's:

```PHP
// Ein Beispiel, um 5 Tage in der Zukunft zu berechnen
$date = date('d-m-Y');
echo "Heute ist ".$date;
$newDate = strtotime('+5 days', strtotime($date));
echo "In 5 Tagen ist: ".date('d-m-Y', $newDate);
```

Ausgabe:
> Heute ist 10-12-2021 <br>
> In 5 Tagen ist: 15-12-2021

## Tief in die Materie:

Das Konzept des Berechnens eines Datums in der Zukunft oder Vergangenheit ist eine Erweiterung des Unix-Zeitstempels. Dieser stellt die Anzahl der Sekunden seit dem 1. Januar 1970 dar und ermöglicht es uns, Zeitoperationen durchzuführen. Alternativ können wir auch die Funktionen `date_add()` und `date_sub()` verwenden, um jeweils eine bestimmte Anzahl von Tagen, Monaten oder Jahren zu einem gegebenen Datum hinzuzufügen oder davon abzuziehen.

## Siehe Auch:

- [PHP Dokumentation zu Date and Time Functions](https://www.php.net/manual/de/ref.datetime.php)
- [Unix-Zeitstempel auf Wikipedia](https://de.wikipedia.org/wiki/Unix-Zeit)
- [Date and Time Manipulation mit PHP von TutsPlus](https://code.tutsplus.com/tutorials/working-with-dates-and-times-in-php--cms-29804)