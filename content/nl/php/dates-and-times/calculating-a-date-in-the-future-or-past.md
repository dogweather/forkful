---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:01.478260-07:00
description: "Een toekomstige of verleden datum berekenen betekent het vinden van\
  \ een datum voor of na een gespecificeerde tijd. Programmeurs doen dit voor\u2026"
lastmod: '2024-03-13T22:44:50.909397-06:00'
model: gpt-4-0125-preview
summary: Een toekomstige of verleden datum berekenen betekent het vinden van een datum
  voor of na een gespecificeerde tijd.
title: Een datum in de toekomst of het verleden berekenen
weight: 26
---

## Wat & Waarom?
Een toekomstige of verleden datum berekenen betekent het vinden van een datum voor of na een gespecificeerde tijd. Programmeurs doen dit voor herinneringen, abonnementen, planning en talloze andere tijdgebonden functies in apps.

## Hoe:
PHP maakt datumberekeningen eenvoudig met `DateTime` en `DateInterval`. Kijk maar:

```PHP
<?php
// De datum van vandaag
$today = new DateTime();
echo $today->format('Y-m-d H:i:s') . "\n";

// Voeg 10 dagen toe
$today->add(new DateInterval('P10D'));
echo $today->format('Y-m-d H:i:s') . "\n";

// Trek 2 maanden af
$today->sub(new DateInterval('P2M'));
echo $today->format('Y-m-d H:i:s') . "\n";
?>
```
Uitvoer zou kunnen zijn:
```
2023-04-01 12:34:56
2023-04-11 12:34:56
2023-02-11 12:34:56
```

## Diepere duik
Vroeger waren PHP-datumberekeningen meer foutgevoelig. `strtotime`, hoewel nog steeds nuttig, kan je in de war brengen met randgevallen. `DateTime` en `DateInterval` brachten precisie en objectgeoriënteerde duidelijkheid.

Alternatieven? Zeker. Bibliotheken zoals Carbon wikkelen PHP's datumfunctionaliteit voor meer leesbaarheid en functies, maar in veel gevallen zullen de ingebouwde klassen van PHP prima volstaan.

Onder de motorkap wijzigen `DateTime::add()` en `DateTime::sub()` het object, dus het is niet nodig om opnieuw toe te wijzen. Ze behandelen tijdeenheid consistent, rekening houdend met zaken als schrikkeljaren en de veranderingen rond zomertijd, wat anders een echte hoofdpijn kan zijn.

## Zie ook
- PHP-handleiding over DateTime: https://www.php.net/manual/en/class.datetime.php
- Documentatie over DateInterval: https://www.php.net/manual/en/class.dateinterval.php
- Carbon: Een eenvoudige API-uitbreiding voor DateTime - https://carbon.nesbot.com
