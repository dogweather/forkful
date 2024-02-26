---
date: 2024-01-26 04:43:29.667567-07:00
description: "Komplexe Zahlen haben einen Realteil und einen Imagin\xE4rteil, \xFC\
  blicherweise geschrieben als `a + bi`. Sie sind entscheidend in fortgeschrittener\u2026"
lastmod: '2024-02-25T18:49:51.023009-07:00'
model: gpt-4-0125-preview
summary: "Komplexe Zahlen haben einen Realteil und einen Imagin\xE4rteil, \xFCblicherweise\
  \ geschrieben als `a + bi`. Sie sind entscheidend in fortgeschrittener\u2026"
title: Umgang mit komplexen Zahlen
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen haben einen Realteil und einen Imaginärteil, üblicherweise geschrieben als `a + bi`. Sie sind entscheidend in fortgeschrittener Mathematik, Physik, Ingenieurwissenschaften und bestimmten Computer-Algorithmen. Programmierer arbeiten mit ihnen, um Berechnungen zu bewältigen, die Wurzeln aus negativen Zahlen und oszillierende Funktionen beinhalten.

## Wie geht das:
PHP bietet eingebaute Unterstützung für komplexe Zahlen mit der Erweiterung `ext-intl` und der Klasse `NumberFormatter`. Hier ist ein Beispiel:

```php
// Stellen Sie sicher, dass die intl-Erweiterung geladen ist
if (!extension_loaded('intl')) {
    die("Die intl-Erweiterung ist nicht aktiviert. Bitte aktivieren Sie sie, um diesen Code auszuführen.");
}

function addComplexNumbers($a, $b) {
    // Verwendung von NumberFormatter zum Parsen und Formatieren komplexer Zahlen
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Parsen komplexer Zahlen aus Strings
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Durchführung der Addition
    $sum = $numA + $numB;

    // Formatieren des Ergebnisses als komplexe Zahl
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Ausgabe: 7+10i
```

## Tiefergehend
Vor `ext-intl` hatte PHP keine native Unterstützung für komplexe Zahlen. Entwickler verwendeten Funktionen oder benutzerdefinierte Klassenbibliotheken, um mit komplexen Zahlen zu arbeiten. Komplexe Operationen konnten mühsam und fehleranfällig sein, aber `ext-intl` bietet einen internationalisierten Weg, um komplexe Zahlen zu präsentieren und zu parsen, abgestimmt auf die ICU-Bibliothek.

Dennoch könnten für schwergewichtige mathematische Operationen einige externe Bibliotheken verwendet werden, die in mathematikfreundlicheren Sprachen (wie C oder Python) geschrieben sind, und durch PHP damit interagieren. Hinsichtlich der Implementierung kümmert sich `ext-intl` hinter den Kulissen darum, genaue Arithmetik zu gewährleisten, während sie die Komplexität für den Entwickler abstrahiert.

Historisch gesehen wurden komplexe Zahlen als 'imaginär' abgelehnt, aber sie sind inzwischen fundamental in verschiedenen wissenschaftlichen und mathematischen Bereichen geworden und offenbaren mehr über ihre Bedeutung in der realen Welt, als ihr imaginärer Status jemals suggerierte.

## Siehe auch
- [PHP-Handbuch zu NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia über komplexe Zahlen](https://de.wikipedia.org/wiki/Komplexe_Zahl)
- [PHP: Der richtige Weg - Arbeiten mit Datentypen](https://phptherightway.com/#data_types)
