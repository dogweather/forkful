---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:15:50.286020-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in PHP ist ein häufig gebrauchtes Verfahren, um zeitbezogene Funktionen zu realisieren – von Timestamps für Log-Einträge bis zu Datumsangaben auf Benutzeroberflächen. Es ist wichtig, weil Zeitstempel oft zentral für die Logik einer Anwendung sind.

## So geht's:
```php
<?php
// Aktuelles Datum und Uhrzeit in der Standard-Formatierung
echo date('Y-m-d H:i:s');

// Aktuelles Datum ohne Uhrzeit
echo date('Y-m-d');

// Zeitstempel der aktuellen Zeit
echo time();
?>
```
Möglicher Output:
```
2023-03-15 15:42:01
2023-03-15
1678902121
```

## Tiefergehende Infos:
Zeit- und Datumsfunktionen sind seit den frühen Versionen von PHP integriert. `date()` und `time()` sind die Basisfunktionen für zeitbezogene Operationen. Es gibt Alternativen wie das DateTime-Objekt, das mit PHP 5.2 eingeführt wurde. DateTime bietet mehr Möglichkeiten und bessere Objektorientierung.

`date()` ist abhängig von der Standard-Zeitzone des Servers, die mit `date_default_timezone_set()` gesetzt werden kann. `time()` gibt einen Unix-Zeitstempel zurück - die Anzahl der Sekunden seit dem 1. Januar 1970 (UTC).

Mit DateTime:
```php
<?php
$jetzt = new DateTime();
echo $jetzt->format('Y-m-d H:i:s');
?>
```
Das gibt uns mehr Flexibilität und Ausnahmebehandlung, und die Möglichkeit, Zeitzonen direkt beim Erstellen des Objekts zu berücksichtigen.

## Weiterführende Links:
- [PHP date() Funktion](https://www.php.net/manual/de/function.date.php)
- [PHP DateTime Klasse](https://www.php.net/manual/de/class.datetime.php)
- [Zeitzonen in PHP](https://www.php.net/manual/de/timezones.php)
