---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:14.190544-07:00
description: 'Wie geht das: #.'
lastmod: '2024-03-13T22:44:53.983050-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Den aktuellen Datum abrufen
weight: 29
---

## Wie geht das:


### Native PHP
Die integrierte `date()`-Funktion von PHP ist der direkteste Weg, um das aktuelle Datum zu erhalten. Sie können das Datum auf verschiedene Weise formatieren, indem Sie den Formatparameter angeben.

```php
echo date("Y-m-d"); // Gibt aus: 2023-04-01 (zum Beispiel)
echo date("l, F j, Y"); // Gibt aus: Samstag, 1. April 2023
```

Um das Datum und die Uhrzeit mit Zeitzonenunterstützung zu erhalten, können Sie die `DateTime`-Klasse zusammen mit `DateTimeZone` verwenden.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Gibt aus: 2023-04-01 12:00:00 (zum Beispiel)
```

### Verwendung von Carbon (Eine Beliebte Drittanbieter-Bibliothek)
[Carbon](https://carbon.nesbot.com/) ist eine einfache API-Erweiterung für `DateTime`, die eine sauberere und fließendere Möglichkeit bietet, mit Daten und Zeiten zu arbeiten.

Stellen Sie zunächst sicher, dass Sie Carbon über Composer installiert haben:
```bash
composer require nesbot/carbon
```

Anschließend können Sie es verwenden, um das aktuelle Datum zu erhalten:

```php
use Carbon\Carbon;

echo Carbon::now(); // Gibt aus: 2023-04-01 12:00:00 (zum Beispiel, im Standardformat)
echo Carbon::now()->toDateString(); // Gibt aus: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Gibt aus: Samstag, 1. April 2023
```

Carbon bereichert die Datum-Zeit-Verarbeitung in PHP durch Hinzufügung von Lesbarkeit und einer Fülle von Funktionalitäten für Zeitmanipulation, -vergleich und -formatierung.
