---
date: 2024-01-26 01:48:19.887057-07:00
description: "Il refactoring \xE8 il processo di ristrutturazione del codice informatico\
  \ esistente senza cambiarne il comportamento esterno. I programmatori eseguono il\u2026"
lastmod: '2024-03-13T22:44:43.524979-06:00'
model: gpt-4-0125-preview
summary: "Il refactoring \xE8 il processo di ristrutturazione del codice informatico\
  \ esistente senza cambiarne il comportamento esterno."
title: Rifattorizzazione
weight: 19
---

## Come fare:
Prendiamo un classico snippet di PHP e applichiamo un po' di magia del refactoring.

Prima del refactoring, il nostro codice potrebbe apparire così:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Articolo: " . $item['name'];
        echo " - Prezzo: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Totale: " . array_sum(array_column($order, 'price'));
    }
}
```

Ma possiamo rifattorizzare questo codice per migliorarne la chiarezza e la modularità:

```php
function printItem($item) {
    echo "Articolo: {$item['name']} - Prezzo: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Totale: " . calculateTotal($order);
    }
}
```
Scomponendo la funzione `printOrderDetails` in funzioni più piccole, il nostro codice diventa più leggibile e più facile da eseguire il debug.

## Approfondimento
Il refactoring ha le sue radici nella comunità di programmazione Smalltalk dei primi anni '90 ed è stato ulteriormente popolarizzato dal libro fondamentale di Martin Fowler "Refactoring: Improving the Design of Existing Code" (1999). Anche se il refactoring può essere applicato a qualsiasi linguaggio di programmazione, la natura dinamica di PHP offre alcune sfide e opportunità uniche.

Alternative al refactoring potrebbero includere la riscrittura del codice da zero, che spesso è più rischiosa e richiede più tempo. Nell'ecosistema PHP, strumenti come PHPStan e Rector possono automaticamente individuare ed eseguire alcune operazioni di refactoring, rispettivamente. Dal punto di vista dell'implementazione, mantenere i refactoring piccoli e testare estensivamente con test unitari sono pratiche fondamentali per garantire un refactoring di successo senza introdurre bug.

## Vedi anche
- Il libro su Refactoring di Martin Fowler: https://martinfowler.com/books/refactoring.html
- PHPStan, uno strumento di analisi statica PHP: https://phpstan.org/
- Rector, uno strumento per il refactoring automatico del codice PHP: https://getrector.org/
- Test unitari con PHPUnit: https://phpunit.de/
