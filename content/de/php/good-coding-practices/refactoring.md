---
date: 2024-01-26 01:47:56.352907-07:00
description: "Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes,\
  \ ohne dessen \xE4u\xDFeres Verhalten zu ver\xE4ndern. Programmierer f\xFChren ein\u2026"
lastmod: '2024-03-13T22:44:53.980978-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes,\
  \ ohne dessen \xE4u\xDFeres Verhalten zu ver\xE4ndern."
title: Refactoring
weight: 19
---

## Wie geht das:
Nehmen wir ein klassisches PHP-Snippet und wenden etwas Refactoring-Magie darauf an.

Vor dem Refactoring könnte unser Code so aussehen:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Preis: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Gesamt: " . array_sum(array_column($order, 'price'));
    }
}
```

Aber wir können diesen Code refaktorisieren, um seine Klarheit und Modularität zu verbessern:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Preis: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Gesamt: " . calculateTotal($order);
    }
}
```
Indem wir die Funktion `printOrderDetails` in kleinere Funktionen aufteilen, wird unser Code lesbarer und einfacher zu debuggen.

## Vertiefung
Das Refactoring hat seine Wurzeln in der Smalltalk-Programmiergemeinschaft der frühen 1990er Jahre und wurde durch das bahnbrechende Buch von Martin Fowler "Refactoring: Improving the Design of Existing Code" (1999) weiter popularisiert. Obwohl Refactoring auf jede Programmiersprache angewendet werden kann, bietet die dynamische Natur von PHP einzigartige Herausforderungen und Möglichkeiten.

Alternativen zum Refactoring könnten das Neuschreiben von Code von Grund auf beinhalten, was oft riskanter und zeitaufwändiger ist. Im PHP-Ökosystem können Tools wie PHPStan und Rector einige Refactoring-Operationen automatisch erkennen und durchführen. Bei der Implementierung ist es wichtig, Refactorings klein zu halten und umfangreich mit Unit-Tests zu testen, um ein erfolgreiches Refactoring ohne die Einführung von Bugs zu gewährleisten.

## Siehe auch
- Martin Fowlers Refactoring-Buch: https://martinfowler.com/books/refactoring.html
- PHPStan, ein PHP-Static-Analysis-Tool: https://phpstan.org/
- Rector, ein Werkzeug für das automatische Refactoring von PHP-Code: https://getrector.org/
- PHP Unit-Tests mit PHPUnit: https://phpunit.de/
