---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:53.444683-07:00
description: "Das Parsen eines Datums aus einem String in PHP beinhaltet das Umwandeln\
  \ von Text, der ein Datum und/oder eine Zeit repr\xE4sentiert, in ein PHP `DateTime`\u2026"
lastmod: '2024-03-13T22:44:53.981985-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String in PHP beinhaltet das Umwandeln\
  \ von Text, der ein Datum und/oder eine Zeit repr\xE4sentiert, in ein PHP `DateTime`\
  \ -Objekt oder andere Datums-/Zeitformate."
title: Einen Datum aus einem String analysieren
weight: 30
---

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
