---
title:    "PHP: Znajdowanie długości ciągu znaków"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości łańcucha znaków jest podstawową i często wykonywaną operacją w programowaniu. Jest to szczególnie przydatne, gdy potrzebujemy sprawdzić, czy długość tekstu nie przekracza określonego limitu, lub gdy musimy wykonać inne operacje na konkretnych fragmentach tekstu.

## Jak to zrobić

```PHP
<?php
// Przykładowy łańcuch znaków
$text = "Witaj, świecie!";

// Użycie funkcji strlen() do znalezienia długości łańcucha
$length = strlen($text);

// Wyświetlenie wyniku
echo "Długość tekstu to " . $length . " znaki.";
?>
```

### Wynik:

Długość tekstu to 15 znaków.

## Głębsza analiza

Funkcja strlen() jest wbudowanym elementem języka PHP i służy do znalezienia długości dowolnego łańcucha znaków. Może być stosowana zarówno dla tekstu wyświetlanego na stronie, jak i tekstu przechowywanego w zmiennych.

Funkcja ta ma kilka cech warte uwagi:

- Zwraca liczbę całkowitą, która odpowiada długości podanego łańcucha.
- Długie i krótkie łańcuchy są obsługiwane w ten sam sposób.
- Nie uwzględnia spacji i innych białych znaków.

Jeśli chcesz poznać więcej szczegółów na temat działania funkcji strlen() oraz jej parametrów, zapraszamy do zapoznania się z [oficjalną dokumentacją PHP](https://www.php.net/manual/en/function.strlen.php).

## Zobacz także

- [Funkcje łańcuchowe w PHP](https://www.php.net/manual/en/ref.strings.php)
- [Jak używać tablic w PHP](https://www.php.net/manual/en/language.types.array.php)
- [Kurs programowania w PHP](https://www.codecademy.com/learn/learn-php)