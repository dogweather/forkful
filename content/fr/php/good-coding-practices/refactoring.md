---
date: 2024-01-26 01:47:50.361082-07:00
description: "Comment faire : Prenons un extrait classique de PHP et appliquons-lui\
  \ de la magie de refactorisation. Avant la refactorisation, notre code pourrait\u2026"
lastmod: '2024-03-13T22:44:57.888052-06:00'
model: gpt-4-0125-preview
summary: Prenons un extrait classique de PHP et appliquons-lui de la magie de refactorisation.
title: 'Refactoring : Mode d''emploi'
weight: 19
---

## Comment faire :
Prenons un extrait classique de PHP et appliquons-lui de la magie de refactorisation.

Avant la refactorisation, notre code pourrait ressembler à cela :

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Prix : " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total : " . array_sum(array_column($order, 'price'));
    }
}
```

Mais nous pouvons refactoriser ce code pour améliorer sa clarté et sa modularité :

```php
function printItem($item) {
    echo "Item: {$item['name']} - Prix : {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total : " . calculateTotal($order);
    }
}
```
En décomposant la fonction `printOrderDetails` en plus petites fonctions, notre code devient plus lisible et plus facile à déboguer.

## Approfondissement
La refactorisation trouve ses racines dans la communauté de programmation smalltalk du début des années 1990 et a été davantage popularisée par le livre séminal de Martin Fowler "Refactoring : Improving the Design of Existing Code" (1999). Bien que la refactorisation puisse être appliquée à n'importe quel langage de programmation, la nature dynamique de PHP permet certains défis et opportunités uniques.

Les alternatives à la refactorisation pourraient inclure la réécriture du code à partir de zéro, ce qui est souvent plus risqué et prend plus de temps. Dans l'écosystème PHP, des outils comme PHPStan et Rector peuvent automatiquement détecter et effectuer certaines opérations de refactorisation, respectivement. En termes de mise en œuvre, garder les refactorisations petites et tester de manière exhaustive avec des tests unitaires sont des pratiques clés pour assurer une refactorisation réussie sans introduire de bogues.

## Voir aussi
- Le livre sur la refactorisation de Martin Fowler : https://martinfowler.com/books/refactoring.html
- PHPStan, un outil d'analyse statique PHP : https://phpstan.org/
- Rector, un outil pour la refactorisation automatique du code PHP : https://getrector.org/
- Tests unitaires PHP avec PHPUnit : https://phpunit.de/
