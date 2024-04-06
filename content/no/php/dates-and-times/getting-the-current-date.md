---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:22.443214-07:00
description: "Hvordan: PHPs innebygde `date()`-funksjon er den mest direkte m\xE5\
  ten \xE5 f\xE5 tak i den gjeldende datoen. Du kan formatere datoen p\xE5 forskjellige\
  \ m\xE5ter ved \xE5\u2026"
lastmod: '2024-03-13T22:44:40.896999-06:00'
model: gpt-4-0125-preview
summary: "PHPs innebygde `date()`-funksjon er den mest direkte m\xE5ten \xE5 f\xE5\
  \ tak i den gjeldende datoen."
title: "F\xE5 dagens dato"
weight: 29
---

## Hvordan:


### Innfødt PHP
PHPs innebygde `date()`-funksjon er den mest direkte måten å få tak i den gjeldende datoen. Du kan formatere datoen på forskjellige måter ved å spesifisere formatparameteren.

```php
echo date("Y-m-d"); // Utgang: 2023-04-01 (for eksempel)
echo date("l, F j, Y"); // Utgang: Lørdag, april 1, 2023
```

For å få datoen og klokkeslettet med støtte for tidssone, kan du bruke `DateTime`-klassen sammen med `DateTimeZone`.

```php
$dateTime = new DateTime('now', new DateTimeZone('America/New_York'));
echo $dateTime->format('Y-m-d H:i:s'); // Utgang: 2023-04-01 12:00:00 (for eksempel)
```

### Ved bruk av Carbon (Et Populært Tredjepartsbibliotek)
[Carbon](https://carbon.nesbot.com/) er en enkel API-utvidelse for `DateTime` som gir en renere og mer flytende måte å jobbe med datoer og klokkeslett.

Først, sørg for at du har Carbon installert via Composer:
```bash
composer require nesbot/carbon
```

Deretter kan du bruke den til å hente den gjeldende datoen:

```php
use Carbon\Carbon;

echo Carbon::now(); // Utgang: 2023-04-01 12:00:00 (for eksempel, i standardformatet)
echo Carbon::now()->toDateString(); // Utgang: 2023-04-01
echo Carbon::now()->format('l, F j, Y'); // Utgang: Lørdag, april 1, 2023
```

Carbon beriker dato- og klokkesletthåndteringen i PHP ved å legge til lesbarhet og en overflod av funksjonalitet for tidsmanipulering, sammenligning, og formatering.
