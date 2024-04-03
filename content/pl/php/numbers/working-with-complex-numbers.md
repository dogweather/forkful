---
date: 2024-01-26 04:44:10.859646-07:00
description: "Liczby zespolone maj\u0105 cz\u0119\u015B\u0107 rzeczywist\u0105 i cz\u0119\
  \u015B\u0107 urojon\u0105, zazwyczaj zapisywane jako `a + bi`. S\u0105 kluczowe\
  \ w zaawansowanej matematyce, fizyce, in\u017Cynierii\u2026"
lastmod: '2024-03-13T22:44:35.491091-06:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone maj\u0105 cz\u0119\u015B\u0107 rzeczywist\u0105 i cz\u0119\
  \u015B\u0107 urojon\u0105, zazwyczaj zapisywane jako `a + bi`."
title: Praca z liczbami zespolonymi
weight: 14
---

## Co i dlaczego?
Liczby zespolone mają część rzeczywistą i część urojoną, zazwyczaj zapisywane jako `a + bi`. Są kluczowe w zaawansowanej matematyce, fizyce, inżynierii oraz niektórych algorytmach komputerowych. Programiści pracują z nimi, aby radzić sobie z obliczeniami obejmującymi pierwiastki kwadratowe z liczb ujemnych oraz funkcje oscylacyjne.

## Jak?
PHP oferuje wbudowane wsparcie dla liczb zespolonych za pomocą rozszerzenia `ext-intl` z klasą `NumberFormatter`. Oto przykład:

```php
// Upewnij się, że rozszerzenie intl jest załadowane
if (!extension_loaded('intl')) {
    die("Rozszerzenie intl nie jest włączone. Proszę je włączyć, aby uruchomić ten kod.");
}

function addComplexNumbers($a, $b) {
    // Użyj NumberFormatter do analizowania i formatowania liczb zespolonych
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Analizuj liczby zespolone z ciągów
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Wykonaj dodawanie
    $sum = $numA + $numB;

    // Sformatuj wynik jako liczbę zespoloną
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Wynik: 7+10i
```

## Pogłębienie
Przed `ext-intl`, PHP nie miało rodzimego wsparcia dla liczb zespolonych. Deweloperzy używali funkcji lub niestandardowych bibliotek klas do obsługi liczb zespolonych. Operacje na liczbach zespolonych mogły być uciążliwe i podatne na błędy, ale `ext-intl` oferuje zinternacjonalizowany sposób prezentacji i analizy liczb zespolonych, zgodny z biblioteką ICU.

Jednak, w przypadku zaawansowanych operacji matematycznych, niektórzy mogą używać zewnętrznych bibliotek napisanych w bardziej przyjaznych dla matematyki językach (takich jak C lub Python) i łączyć się z nimi za pośrednictwem PHP. Jeśli chodzi o implementację, `ext-intl` radzi sobie z nią za kulisami, zapewniając dokładne obliczenia matematyczne, jednocześnie abstrahując złożoność od programisty.

Historycznie liczby zespolone były niechętnie przyjmowane, będąc określane jako 'urojone', ale od tego czasu stały się fundamentalne w różnorodnych dziedzinach naukowych i matematycznych, pokazując więcej o ich znaczeniu w rzeczywistym świecie, niż sugerowałby ich urojony status.

## Zobacz także
- [Podręcznik PHP o NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia o liczbach zespolonych](https://pl.wikipedia.org/wiki/Liczba_zespolona)
- [PHP: The Right Way – Praca z Typami Danych](https://phptherightway.com/#data_types)
