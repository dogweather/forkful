---
date: 2024-01-26 01:48:37.086197-07:00
description: "Refaktorering er prosessen med \xE5 omstrukturere eksisterende dataprogramkode\
  \ uten \xE5 endre dens eksterne oppf\xF8rsel. Programmerere refaktorerer for \xE5\
  \u2026"
lastmod: '2024-03-13T22:44:40.894966-06:00'
model: gpt-4-0125-preview
summary: "Refaktorering er prosessen med \xE5 omstrukturere eksisterende dataprogramkode\
  \ uten \xE5 endre dens eksterne oppf\xF8rsel. Programmerere refaktorerer for \xE5\
  \u2026"
title: Refaktorering
weight: 19
---

## Hva & Hvorfor?
Refaktorering er prosessen med å omstrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere refaktorerer for å forbedre de ikke-funksjonelle attributtene til programvaren, ved å gjøre koden renere, mer effektiv og lettere å vedlikeholde.

## Hvordan:
La oss ta et klassisk PHP-utdrag og bruke litt refaktoreringstrollkunst på det.

Før refaktorering kan koden vår se slik ut:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Vare: " . $item['name'];
        echo " - Pris: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Totalt: " . array_sum(array_column($order, 'price'));
    }
}
```

Men vi kan refaktorere denne koden for å forbedre dens klarhet og modularitet:

```php
function printItem($item) {
    echo "Vare: {$item['name']} - Pris: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Totalt: " . calculateTotal($order);
    }
}
```
Ved å bryte ned `printOrderDetails`-funksjonen til mindre funksjoner, blir koden vår mer lesbart og lettere å feilsøke.

## Dypdykk
Refaktorering har sine røtter i smalltalk-programmeringssamfunnet fra tidlig på 1990-tallet og ble ytterligere popularisert av Martin Fowlers banebrytende bok "Refaktorering: Forbedring av designet på eksisterende kode" (1999). Selv om refaktorering kan anvendes på hvilket som helst programmeringsspråk, tillater PHPs dynamiske natur noen unike utfordringer og muligheter.

Alternativer til refaktorering kan inkludere å skrive om koden fra bunnen av, noe som ofte er risikabelt og mer tidkrevende. I PHP-økosystemet kan verktøy som PHPStan og Rector automatisk identifisere og utføre noen refaktoreringsoperasjoner, henholdsvis. Når det gjelder implementasjon, er det å holde refaktoreringene små og teste omfattende med enhetstester nøkkelpraksiser for å sikre vellykket refaktorering uten å introdusere feil.

## Se Også
- Martin Fowlers bok om Refaktorering: https://martinfowler.com/books/refactoring.html
- PHPStan, et verktøy for statisk analyse av PHP: https://phpstan.org/
- Rector, et verktøy for automatisk refaktorering av PHP-kode: https://getrector.org/
- PHP-enhetstesting med PHPUnit: https://phpunit.de/
