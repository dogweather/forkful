---
title:                "PHP: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego generowanie losowych liczb jest ważne?

Generowanie losowych liczb jest bardzo ważne w programowaniu, ponieważ często potrzebujemy w naszych aplikacjach wylosowanych wartości. Może to być przydatne w grach, symulacjach, a nawet do testowania kodu.

## Jak to zrobić?

W PHP mamy dostęp do funkcji `rand(min, max)`, która zwraca losową liczbę całkowitą w podanym zakresie. Przykład użycia:

```PHP
<?php
$losowa_liczba = rand(1, 10);
echo "Wylosowano liczbę: $losowa_liczba";
?>
```

Powyższy kod może zwrócić wartość od 1 do 10, w zależności od wywołania funkcji. Jednakże, jeśli chcemy wygenerować liczbę zmiennoprzecinkową, musimy użyć funkcji `mt_rand(min, max)`. Przykład:

```PHP
<?php
$losowa_liczba = mt_rand(1, 100) / 10;
echo "Wylosowano liczbę zmiennoprzecinkową: $losowa_liczba";
?>
```

W powyższym kodzie, wylosowana liczba zostanie przemnożona przez 0.1, co spowoduje, że otrzymamy liczbę zmiennoprzecinkową z zakresu 0.1 do 10.

## Głębszy zanurzenie

W PHP istnieje również funkcja `shuffle()`, która pozwala zmienić kolejność elementów w tablicy w sposób losowy. Przykład użycia:

```PHP
<?php
$tablica = array("czerwony", "zielony", "niebieski", "żółty", "fioletowy");
shuffle($tablica);
echo "Wylosowana kolejność kolorów: " . implode(", ", $tablica);
?>
```

Funkcja `mt_rand()` jest również szybsza i bardziej losowa niż funkcja `rand()`. Jest ona oparta na generatorze liczb pseudolosowych Mersenne Twister, który jest uważany za jeden z najlepszych generatorów.

## Zobacz również

- Dokumentacja PHP na temat funkcji [rand()](https://www.php.net/manual/en/function.rand.php)
- Dokumentacja PHP na temat funkcji [mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- Dokumentacja PHP na temat funkcji [shuffle()](https://www.php.net/manual/en/function.shuffle.php)