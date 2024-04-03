---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:18.306171-07:00
description: "Het parsen van een datum uit een string betekent het converteren van\
  \ tekst die een datum en tijd vertegenwoordigt naar een programmeerbaar formaat.\u2026"
lastmod: '2024-03-13T22:44:50.905231-06:00'
model: gpt-4-0125-preview
summary: Het parsen van een datum uit een string betekent het converteren van tekst
  die een datum en tijd vertegenwoordigt naar een programmeerbaar formaat.
title: Een datum uit een string parsen
weight: 30
---

## Hoe te:
PHP maakt het parsen van datums uit strings vrij eenvoudig met de `DateTime` klasse. Hier is een snel voorbeeld:

```php
<?php
$dateString = '2023-04-12 14:00:00';
$dateTime = new DateTime($dateString);

echo $dateTime->format('Y-m-d H:i:s'); // Geeft uit: 2023-04-12 14:00:00
?>
```

Eenvoudig, toch? Nu, wil je het formaat of de tijdzone wijzigen? Zo doe je dat:

```php
<?php
$dateString = '12 april 2023 14:00:00';
$dateTime = DateTime::createFromFormat('F j, Y H:i:s', $dateString);
$dateTime->setTimezone(new DateTimeZone('Europe/London'));

echo $dateTime->format('Y-m-d H:i'); // Geeft uit: 2023-04-12 14:00
?>
```

Speel met formaten en tijdzones om te zien hoe krachtig dit kan zijn.

## Diepgaand
Historisch gezien moesten PHP-ontwikkelaars handmatig datumstrings parsen of `strtotime()` gebruiken, wat effectief is maar minder krachtig dan `DateTime`. Geïntroduceerd in PHP 5.2.0, biedt `DateTime` object-georiënteerde datum-/tijdmanipulatie.

Waarom de verandering? Omdat `DateTime`:

1. Uitzonderingen afhandelt.
2. Werkt met verschillende kalenders.
3. Tijdzonebewust is.
4. Meer opmaak- en parseeropties heeft.

Alternatieven zijn de `IntlDateFormatter` voor internationalisering of de `Carbon` bibliotheek voor moderne syntactische hulpmiddelen.

Let op de valkuilen bij het parsen:

- Valideer altijd input. Incorrecte formaten veroorzaken verkeerde datums of fouten.
- Tijdzones zijn belangrijk. Sla op in UTC en toon lokaal.
- Schrikkelseconden en zomertijden kunnen berekeningen beïnvloeden.

## Zie Ook
- [PHP Handleiding over DateTime](https://www.php.net/manual/en/class.datetime.php)
- [PHP Datum- en Tijdfuncties](https://www.php.net/manual/en/ref.datetime.php)
- [Carbon: Een eenvoudige PHP API-extensie voor DateTime](https://carbon.nesbot.com/)
