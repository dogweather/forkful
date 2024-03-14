---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:19.849335-07:00
description: "Att f\xE5 det aktuella datumet i PHP \xE4r en grundl\xE4ggande uppgift\
  \ som g\xF6r det m\xF6jligt f\xF6r dig att h\xE4mta och manipulera systemets datum\
  \ och tid. Detta \xE4r\u2026"
lastmod: '2024-03-13T22:44:38.006632-06:00'
model: gpt-4-0125-preview
summary: "Att f\xE5 det aktuella datumet i PHP \xE4r en grundl\xE4ggande uppgift som\
  \ g\xF6r det m\xF6jligt f\xF6r dig att h\xE4mta och manipulera systemets datum och\
  \ tid. Detta \xE4r\u2026"
title: "F\xE5 det aktuella datumet"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få det aktuella datumet i PHP är en grundläggande uppgift som gör det möjligt för dig att hämta och manipulera systemets datum och tid. Detta är avgörande för funktioner som loggning, tidstämpling av inlägg, schemaläggning av händelser eller utförande av tidskänsliga operationer i dina applikationer.

## Hur gör man:
### Inbyggd PHP
PHP:s inbyggda `date()`-funktion är det mest direkta sättet att få det aktuella datumet. Du kan formatera datumet på olika sätt genom att specificera formatparametern.

```php
echo date("Y-m-d"); // Ger ut: 2023-04-01 (till exempel)
echo date("l, F j, Y"); // Ger ut: lördag, april 1, 2023
```

För att få datum och tid med tidszonsstöd kan du använda `DateTime`-klassen tillsammans med `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Ger ut: 2023-04-01 12:00:00 (till exempel)
```

### Använda Carbon (Ett populärt tredjepartsbibliotek)
[Carbon](https://carbon.nesbot.com/) är en enkel API-förlängning för `DateTime` som erbjuder ett renare och mer flytande sätt att arbeta med datum och tider.

Se först till att du har Carbon installerat via Composer:
```bash
composer require nesbot/carbon
```

Sedan kan du använda det för att få det aktuella datumet:

```php
use Carbon\Carbon;

echo Carbon::now(); // Ger ut: 2023-04-01 12:00:00 (till exempel, i det förinställda formatet)
echo Carbon::now()->toDateString(); // Ger ut: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Ger ut: lördag, april 1, 2023
```

Carbon berikar hanteringen av datum-tid i PHP genom att lägga till läsbarhet och en skattkista av funktionalitet för tidsmanipulering, jämförelse och formatering.
