---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:38.135362-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Generowanie losowych liczb to proces tworzenia nieprzewidywalnych wartości. Programiści wykorzystują to do symulacji, testów, gier, i gdziekolwiek niezbędna jest element losowości.

## Jak to zrobić:
```PHP
<?php
// Proste losowanie liczby:
echo rand() . "\n"; // Wyświetla losową liczbę

// Losowanie liczby z zakresu:
echo rand(1, 100) . "\n"; // Losowa liczba od 1 do 100

// Dla lepszej jakości losowania (PHP 7+):
echo random_int(1, 100) . "\n"; // Bezpieczniejsza losowa liczba od 1 do 100
?>
```
Przykładowe wyjście:
```
2123456789
57
23
```

## Deep Dive
Generowanie liczb losowych w PHP zaczęło się od funkcji `rand()`, która jednak nie zawsze była idealna pod względem jakości generowanych liczb. Z czasem, w wersji PHP 7, wprowadzono `random_int()`, zapewniającą lepszą jakość i bezpieczeństwo (kryptograficznie silne generowanie liczb losowych). Warto też wspomnieć o alternatywie `mt_rand()`, która jest szybsza i często ma lepsze właściwości statystyczne niż `rand()` ale nie jest tak bezpieczna jak `random_int()`.

## Zobacz również:
- [PHP Manual on random_int](https://www.php.net/manual/en/function.random-int.php)
- [PHP Manual on mt_rand](https://www.php.net/manual/en/function.mt-rand.php)
- Secure PHP random numbers guide: [Paragon Initiative Enterprises blog](https://paragonie.com/blog/2016/05/how-generate-secure-random-numbers-in-various-programming-languages)