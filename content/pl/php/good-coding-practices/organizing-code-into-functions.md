---
aliases:
- /pl/php/organizing-code-into-functions/
date: 2024-01-26 01:11:54.163439-07:00
description: "Organizowanie kodu w funkcje polega na dzieleniu go na ponownie u\u017C\
  ywalne bloki o okre\u015Blonych celach. Robimy to, aby utrzyma\u0107 porz\u0105\
  dek, zapobiega\u0107\u2026"
lastmod: 2024-02-18 23:08:49.700952
model: gpt-4-1106-preview
summary: "Organizowanie kodu w funkcje polega na dzieleniu go na ponownie u\u017C\
  ywalne bloki o okre\u015Blonych celach. Robimy to, aby utrzyma\u0107 porz\u0105\
  dek, zapobiega\u0107\u2026"
title: Organizacja kodu w funkcje
---

{{< edit_this_page >}}

## Co i dlaczego?
Organizowanie kodu w funkcje polega na dzieleniu go na ponownie używalne bloki o określonych celach. Robimy to, aby utrzymać porządek, zapobiegać redundancji i ułatwić debugowanie.

## Jak to zrobić:
Wyobraź sobie, że mamy powtarzający się kod pozdrawiający użytkowników. Zamiast tego opakujemy go w funkcję o nazwie `greet_user`:

```php
function greet_user($name) {
    return "Witaj, " . $name . "!";
}

echo greet_user("Alicja");
echo greet_user("Bob");
```

Wyjście:
```
Witaj, Alicja!
Witaj, Bob!
```

Teraz masz poręczne narzędzie, którego możesz używać kiedy tylko chcesz, bez potrzeby przepisywania tych samych linijek kodu za każdym razem, kiedy chcesz powiedzieć cześć.

## Dogłębna analiza
Funkcje pojawiają się w programowaniu od wczesnych dni języka FORTRAN w latach 50-tych. Są kamieniem węgielnym strukturalnego programowania i dotyczą modularności i izolacji. Alternatywy? Możesz zastosować podejście obiektowe i mówić o klasach i metodach, które są funkcjami w eleganckim przebraniu. Jeśli chodzi o PHP, szczegóły implementacji obejmują określanie wartości domyślnych dla parametrów, wskazówki typów dla wejść oraz możliwość zwracania wielu wartości za pomocą tablicy lub, począwszy od PHP 7.1, listy.

Oto nowoczesne podejście z deklaracją typu i wartościami domyślnymi:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 wprowadziło również funkcje strzałkowe, które ułatwiają pisanie zwięzłych funkcji jednoliniowych, powszechnie używanych w operacjach na tablicach:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Wyjście:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Zobacz również
- [Podręcznik PHP na temat funkcji](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: The Right Way - Funkcje](https://phptherightway.com/#functions)
- [Dowiedz się więcej o funkcjach strzałkowych w PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
