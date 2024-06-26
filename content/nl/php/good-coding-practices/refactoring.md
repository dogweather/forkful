---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:01.697187-07:00
description: 'Hoe: Laten we een klassiek PHP-fragment nemen en er wat refactoring
  magie op toepassen. Voor de refactoring kan onze code er zo uitzien.'
lastmod: '2024-03-13T22:44:50.904201-06:00'
model: gpt-4-0125-preview
summary: Laten we een klassiek PHP-fragment nemen en er wat refactoring magie op toepassen.
title: Refactoring
weight: 19
---

## Hoe:
Laten we een klassiek PHP-fragment nemen en er wat refactoring magie op toepassen.

Voor de refactoring kan onze code er zo uitzien:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Prijs: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Totaal: " . array_sum(array_column($order, 'price'));
    }
}
```

Maar we kunnen deze code refactoren om de duidelijkheid en modulariteit ervan te verbeteren:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Prijs: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Totaal: " . calculateTotal($order);
    }
}
```
Door de functie `printOrderDetails` op te splitsen in kleinere functies wordt onze code leesbaarder en makkelijker te debuggen.

## Diepgaand
Refactoring heeft zijn wortels in de Smalltalk-programmeergemeenschap van begin jaren '90 en werd verder gepopulariseerd door Martin Fowler's baanbrekende boek "Refactoring: Improving the Design of Existing Code" (1999). Hoewel refactoring op elke programmeertaal toegepast kan worden, biedt de dynamische aard van PHP unieke uitdagingen en kansen.

Alternatieven voor refactoring kunnen onder andere het volledig herschrijven van code omvatten, wat vaak riskanter en tijdrovender is. In het PHP-ecosysteem kunnen tools zoals PHPStan en Rector automatisch bepaalde refactoring-operaties opsporen en uitvoeren. Wat betreft de implementatie, het klein houden van refactorings en het uitgebreid testen met unit tests zijn cruciale praktijken om succesvolle refactoring te waarborgen zonder bugs te introduceren.

## Zie Ook
- Martin Fowler's Refactoring boek: https://martinfowler.com/books/refactoring.html
- PHPStan, een PHP statische analyse tool: https://phpstan.org/
- Rector, een tool voor automatische refactoring van PHP code: https://getrector.org/
- PHP Unit Testing met PHPUnit: https://phpunit.de/
