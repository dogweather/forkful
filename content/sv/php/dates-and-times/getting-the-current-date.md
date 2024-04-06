---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:19.849335-07:00
description: "Hur g\xF6r man: PHP:s inbyggda `date()`-funktion \xE4r det mest direkta\
  \ s\xE4ttet att f\xE5 det aktuella datumet. Du kan formatera datumet p\xE5 olika\
  \ s\xE4tt genom att\u2026"
lastmod: '2024-03-13T22:44:38.006632-06:00'
model: gpt-4-0125-preview
summary: "PHP:s inbyggda `date()`-funktion \xE4r det mest direkta s\xE4ttet att f\xE5\
  \ det aktuella datumet."
title: "F\xE5 det aktuella datumet"
weight: 29
---

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
