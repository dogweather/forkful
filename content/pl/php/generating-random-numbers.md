---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Generowanie liczb losowych to technika, która tworzy ciąg liczb, które nie mają z góry zdefiniowanej kolejności lub wzoru. Programiści robią to, aby dodać element niewiadomej do swoich programów, co jest szczególnie użyteczne w grach, symulacjach i testach.

## Jak to zrobić:

Możemy wygenerować liczbę losową w PHP (wersjat 8.0.2) używając funkcji rand(). Oto przykład:

```PHP
<?php
echo rand() . "\n";
echo rand(5, 15);
?>
```
Wynik:

```PHP
12345
7
```
Pierwsza linia kodu generuje dowolną liczbę losową, a druga linia generuje liczbę losową pomiędzy 5 a 15.

## Głębsze spojrzenie:

Historia generowania liczb pseudolosowych sięga roku 1946, kiedy John Von Neumann zaproponował metodę środkowego kwadratu. Do dzisiaj rozwijane są nowe metody, takie jak LFSR czy Twister Mersenne'a.

Alternatywą dla rand() w PHP jest funkcja mt_rand(), która używa generatora liczb pseudolosowych Twister Mersenne'a. Co więcej, PHP 7.0 wprowadził funkcję random_int(), która jest bezpieczna kryptograficznie.

Co do szczegółów implementacji, rand() generuje liczby na podstawie wzoru (a*X + c) mod m. W PHP, 'm' to RAND_MAX, domyślnie ustawiony na wartość 32767.

## Zobacz też:

Do pogłębienia wiedzy na temat generowania liczb losowych, polecam następujące źródła:
- PHP Manual, funkcja rand(): https://www.php.net/manual/en/function.rand.php
- Wikipedia, Generowanie liczb pseudolosowych: https://pl.wikipedia.org/wiki/Generowanie_liczb_pseudolosowych
- PHP Manual, funkcja mt_rand(): https://www.php.net/manual/en/function.mt-rand.php
- PHP Manual, funkcja random_int(): https://www.php.net/manual/en/function.random-int.php