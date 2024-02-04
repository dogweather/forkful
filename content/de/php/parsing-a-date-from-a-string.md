---
title:                "Einen Datum aus einem String analysieren"
date:                  2024-02-03T19:14:53.444683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen Datum aus einem String analysieren"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String in PHP beinhaltet das Umwandeln von Text, der ein Datum und/oder eine Zeit repräsentiert, in ein PHP `DateTime` -Objekt oder andere Datums-/Zeitformate. Dies ist entscheidend für die Validierung, Manipulation, Speicherung und Darstellung von Daten, insbesondere wenn man mit Benutzereingaben oder Daten aus externen Quellen arbeitet.

## Wie geht's:

Die eingebaute `DateTime`-Klasse von PHP bietet einen leistungsstarken Satz von Funktionen zum Parsen und Arbeiten mit Daten. Sie können eine `DateTime`-Instanz aus einem Datumstring mithilfe des Konstruktors erstellen und dann nach Bedarf formatieren. So geht's:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Ausgabe: 2023-04-25 15:30:00
```

Um Strings zu verarbeiten, die nicht standardisierte Formate folgen, können Sie die Methode `createFromFormat` verwenden, die es Ihnen ermöglicht, das genaue Format des Eingabedatums anzugeben:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Ausgabe: 2023-04-25 15:30:00
```

Für komplexeres Parsen, das möglicherweise nicht direkt von `DateTime` unterstützt wird, bietet PHP die Funktion `strtotime`, die versucht, jede englische textuelle Datums- und Zeitbeschreibung in einen Unix-Zeitstempel zu parsen:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// Die Ausgabe variiert je nach aktuellem Datum, z.B. "2023-05-04"
```

**Verwendung von Drittanbieter-Bibliotheken:**

Obwohl PHPs eingebaute Funktionen eine breite Palette von Anwendungsfällen abdecken, benötigen Sie manchmal möglicherweise ausgefeiltere Parsing-Fähigkeiten. Die Carbon-Bibliothek, eine Erweiterung der PHP-DateTime-Klasse, bietet einen reichhaltigen Satz von Funktionen für die Datums-/Zeitmanipulation:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// Die Ausgabe variiert, z.B. "2023-04-26 00:00:00"
```

Carbons `parse`-Methode kann geschickt eine Vielzahl von Daten- und Zeitformaten handhaben, was es zu einem unschätzbaren Werkzeug für Anwendungen macht, die flexible Datums-Parsing-Funktionalität erfordern.
