---
title:                "Datum in einen String umwandeln"
date:                  2024-01-20T17:36:57.306406-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Datum als String zu konvertieren bedeutet, es in eine lesbare Textform umzuwandeln – praktisch, wenn Daten für Menschen aufbereitet oder in einem bestimmten Format gespeichert werden müssen.

## So geht's:
PHP bietet die `date()` Funktion, um Datumsobjekte in Strings umzuwandeln. Das erste Argument ist das Format, das zweite das zu konvertierende Datum als Unix-Timestamp.

```php
echo date('Y-m-d'); // Aktuelles Datum im Format JJJJ-MM-TT
echo date('d.m.Y H:i:s', time()); // Aktuelle Zeit mit Stunden, Minuten, Sekunden
```

Ausgabe könnte sein:
```
2023-04-01
01.04.2023 15:42:07
```

## Tiefgang:
In den Anfängen von PHP gab es die Funktionen `date()` und `strtotime()` für alle Datums- und Zeitbedürfnisse. Mit PHP 5.2.0 kam das DateTime-Objekt, eine objektorientierte Alternative. Hiermit kann man Datum und Uhrzeit flexibler bearbeiten und ausgeben. Nicht zu vergessen sind die verschiedenen Zeitzonen, die mit DateTime exakter gehandhabt werden können.

```php
$datum = new DateTime();
echo $datum->format('Y-m-d H:i:s'); // OOP-Weg, das aktuelle Datum zu bekommen
```

Ältere Funktionen wie `strftime()` sind mittlerweile veraltet, aber noch da für Legacy-Code. Ebenso gibt es `DateTimeImmutable` für unveränderliche Datumswerte, um Seiteneffekte zu vermeiden.

## Siehe auch:
- Offizielle PHP-Dokumentation zur `date()`-Funktion: [php.net/manual/de/function.date.php](https://www.php.net/manual/de/function.date.php)
- PHP DateTime-Klasse: [php.net/manual/de/class.datetime.php](https://www.php.net/manual/de/class.datetime.php)
- Überblick über Datum und Zeit in PHP: [php.net/manual/de/book.datetime.php](https://www.php.net/manual/de/book.datetime.php)
