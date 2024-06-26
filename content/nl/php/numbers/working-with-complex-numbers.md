---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:57.448499-07:00
description: 'Hoe: PHP biedt ingebouwde ondersteuning voor complexe getallen via de
  `ext-intl` extensie met de `NumberFormatter` klasse. Hier is een voorbeeld.'
lastmod: '2024-03-13T22:44:50.888645-06:00'
model: gpt-4-0125-preview
summary: PHP biedt ingebouwde ondersteuning voor complexe getallen via de `ext-intl`
  extensie met de `NumberFormatter` klasse.
title: Werken met complexe getallen
weight: 14
---

## Hoe:
PHP biedt ingebouwde ondersteuning voor complexe getallen via de `ext-intl` extensie met de `NumberFormatter` klasse. Hier is een voorbeeld:

```php
// Zorg dat de intl extensie geladen is
if (!extension_loaded('intl')) {
    die("De intl extensie is niet ingeschakeld. Schakel deze in om deze code te kunnen uitvoeren.");
}

function addComplexNumbers($a, $b) {
    // Gebruik NumberFormatter om complexe getallen te analyseren en te formatteren
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Parse complexe getallen uit strings
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Voer de optelling uit
    $sum = $numA + $numB;

    // Formatteer het resultaat als een complex getal
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Output: 7+10i
```

## Diepgaande Informatie
Voor `ext-intl` had PHP geen native ondersteuning voor complexe getallen. Ontwikkelaars gebruikten functies of aangepaste klassebibliotheken om complexe getallen te hanteren. Complexe bewerkingen konden omslachtig en foutgevoelig zijn, maar `ext-intl` biedt een geïnternationaliseerde manier om complexe getallen te presenteren en te analyseren, in lijn met de ICU-bibliotheek.

Echter, voor zware wiskundige bewerkingen, kunnen sommigen externe bibliotheken gebruiken die geschreven zijn in meer wiskundevriendelijke talen (zoals C of Python) en ermee communiceren via PHP. Wat betreft de implementatie, `ext-intl` handelt het achter de schermen af, zorgt voor nauwkeurige aritmetiek terwijl het de complexiteit voor de ontwikkelaar verbergt.

Historisch gezien werden complexe getallen met enig wantrouwen bekeken, als zijnde 'imaginair', maar ze zijn sindsdien fundamenteel geworden in diverse wetenschappelijke en wiskundige velden, en onthullen meer over hun betekenis in de echte wereld dan hun imaginaire status ooit suggereerde.

## Zie Ook
- [PHP-handleiding over NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia over complexe getallen](https://nl.wikipedia.org/wiki/Complex_getal)
- [PHP: De Juiste Manier - Werken met Datatypes](https://phptherightway.com/#data_types)
