---
date: 2024-01-26 01:50:00.602784-07:00
description: "Refaktoryzacja to proces restrukturyzacji istniej\u0105cego kodu komputerowego,\
  \ bez zmiany jego zewn\u0119trznego zachowania. Programi\u015Bci przeprowadzaj\u0105\
  \u2026"
lastmod: '2024-03-11T00:14:08.691869-06:00'
model: gpt-4-0125-preview
summary: "Refaktoryzacja to proces restrukturyzacji istniej\u0105cego kodu komputerowego,\
  \ bez zmiany jego zewn\u0119trznego zachowania. Programi\u015Bci przeprowadzaj\u0105\
  \u2026"
title: Refaktoryzacja
---

{{< edit_this_page >}}

## Co i dlaczego?
Refaktoryzacja to proces restrukturyzacji istniejącego kodu komputerowego, bez zmiany jego zewnętrznego zachowania. Programiści przeprowadzają refaktoryzację, aby poprawić nie-funkcjonalne atrybuty oprogramowania, czyniąc kod czytelniejszym, bardziej wydajnym i łatwiejszym w utrzymaniu.

## Jak to zrobić:
Weźmy klasyczny fragment kodu PHP i zastosujmy trochę magii refaktoryzacji.

Przed refaktoryzacją nasz kod może wyglądać tak:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Przedmiot: " . $item['name'];
        echo " - Cena: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Razem: " . array_sum(array_column($order, 'price'));
    }
}
```

Ale możemy zrefaktoryzować ten kod, aby poprawić jego klarowność i modularność:

```php
function printItem($item) {
    echo "Przedmiot: {$item['name']} - Cena: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Razem: " . calculateTotal($order);
    }
}
```
Poprzez podział funkcji `printOrderDetails` na mniejsze funkcje, nasz kod staje się bardziej czytelny i łatwiejszy do debugowania.

## Wgłębiając się
Refaktoryzacja ma swoje korzenie w społeczności programowania smalltalk na początku lat 90. XX wieku i została dodatkowo spopularyzowana przez książkę Martina Fowlera "Refaktoryzacja: Ulepszanie projektu istniejącego kodu" (1999). Chociaż refaktoryzację można zastosować do dowolnego języka programowania, dynamiczna natura PHP pozwala na niektóre unikalne wyzwania i możliwości.

Alternatywami dla refaktoryzacji mogą być przepisywanie kodu od nowa, co często jest bardziej ryzykowne i czasochłonne. W ekosystemie PHP, narzędzia takie jak PHPStan i Rector mogą automatycznie wykrywać i przeprowadzać niektóre operacje refaktoryzacyjne. Pod względem implementacji, kluczowymi praktykami zapewniającymi udaną refaktoryzację bez wprowadzania błędów są: przeprowadzanie niewielkich zmian i intensywne testowanie z wykorzystaniem testów jednostkowych.

## Zobacz również
- Książka Martina Fowlera o refaktoryzacji: https://martinfowler.com/books/refactoring.html
- PHPStan, narzędzie do statycznej analizy PHP: https://phpstan.org/
- Rector, narzędzie do automatycznej refaktoryzacji kodu PHP: https://getrector.org/
- Testowanie jednostkowe PHP z PHPUnit: https://phpunit.de/
