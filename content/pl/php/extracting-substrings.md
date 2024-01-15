---
title:                "Wydobywanie podłańcuchów"
html_title:           "PHP: Wydobywanie podłańcuchów"
simple_title:         "Wydobywanie podłańcuchów"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często podczas tworzenia skryptów w PHP, jesteśmy zmuszeni do manipulacji ciągami znaków. Jednym z powszechnych zadań jest wyciąganie fragmentów (substring) z istniejącego ciągu znaków. W tym artykule dowiesz się, dlaczego i jak to zrobić w PHP.

## Jak to zrobić

Aby wyciągnąć fragment ciągu znaków w PHP, musimy użyć funkcji `substr()`. Jej składnia jest następująca: `substr($string, $start, $length)`, gdzie `$string` to ciąg znaków, z którego chcemy wyciągnąć fragment, `$start` to indeks początkowy, a `$length` to długość wyciąganego fragmentu. Na przykład:

```PHP
<?php
$string = "Hello World";
echo substr($string, 0, 5); // wypisze "Hello"
```

Jeśli chcemy wyciągnąć fragment od końca ciągu, możemy użyć ujemnego indeksu. Na przykład `$start = -5` oznacza, że będziemy zaczynać od piątego znaku od końca. Możemy również pominąć parametr `$length`, wtedy zostanie wyciągnięty cały dostępny fragment od wybranego indeksu. Przykład:

```PHP
<?php
$string = "Hello World";
echo substr($string, -5); // wypisze "World"
```

## Deep Dive

Funkcja `substr()` może również służyć do podstawiania znaków w ciągu. W tym celu możemy podać trzeci parametr, który będzie traktowany jako znaki, które chcemy wstawić w wybranym miejscu. Przykład:

```PHP
<?php
$string = "Hello World";
echo substr($string, 6, 0, "to the "); // wypisze "Hello to the World"
```

Dodatkowo, możemy użyć funkcji `strpos()` do znalezienia indeksu danego słowa lub znaku w ciągu, a następnie użyć tej wartości jako parametr `$start`. Przykład:

```PHP
<?php
$string = "Hello World";
$start = strpos($string, "World"); // zwraca 6
echo substr($string, $start, 5); // wypisze "World"
```

## Zobacz również

Jeśli interesuje Cię więcej o ciągach znaków w PHP, zajrzyj do dokumentacji na oficjalnej stronie PHP lub przeczytaj artykuł o wyrażeniach regularnych w PHP.