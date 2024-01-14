---
title:                "PHP: Generowanie losowych liczb"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest jednym z podstawowych elementów programowania. Może być przydatne do tworzenia gier, generowania losowych haseł lub innych zastosowań wymagających różnorodnych danych. W tym artykule dowiesz się, jak w łatwy sposób wygenerować losowe liczby w języku PHP.

## Jak to zrobić

Aby wygenerować losową liczbę w PHP, możesz użyć funkcji `rand(min, max)`, gdzie min oznacza minimalną wartość, a max maksymalną. Na przykład, jeśli chcesz wygenerować liczbę od 1 do 10, użyjemy `rand(1, 10)`. Oto przykładowy kod:

```PHP 
<?php
echo rand(1, 10); //wygeneruje losową liczbę od 1 do 10
```

Jeśli potrzebujesz wygenerować losową liczbę z określonym krokiem, można użyć funkcji `rand(min, max)/step`, na przykład `rand(1, 10)/2` wygeneruje liczby od 1 do 10, ale tylko w krokach co 2.

Możesz również wybrać losowy element z tablicy, używając funkcji `array_rand(array)`. Na przykład:

```PHP
<?php
$colors = array("red", "blue", "yellow", "green"); //tablica z kolorami
echo $colors[array_rand($colors)]; //wybierze losowy kolor z tablicy
```

Istnieje wiele innych funkcji w PHP, które pomogą Ci wygenerować losowe liczby w różnych formatach, na przykład `mt_rand()`, `rand()`, `rand(min, max)/step`, `mt_rand(0, pow(10, 16))` i wiele innych. Sprawdź dokumentację, aby poznać pełną listę możliwości.

## Deep Dive

Jeśli chcesz wygenerować losową liczbę w sposób bardziej przewidywalny, można użyć `mt_rand()` zastosowanie mersenne twister. Jest to algorytm generujący liczby pseudolosowe, które są bardziej losowe niż domyślna metoda `rand()`.

Możesz również wygenerować losowe liczby w oparciu o datę lub czas. Na przykład, aby wygenerować losową liczbę w zależności od aktualnego czasu, można użyć funkcji `rand(date('H'), date('H')+1)`.

Pamiętaj jednak, że wygenerowane liczby przez te funkcje nie są całkowicie losowe, są one generowane za pomocą algorytmów, które mają pewne zasady i powtarzają się po pewnym czasie. Zawsze należy uważać przy tworzeniu aplikacji, które wymagają silnego i całkowicie losowego generowania liczb.

## Zobacz również

- [Dokumentacja PHP: Generowanie liczb pseudolosowych](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP: Funkcja array_rand()](https://www.php.net/manual/en/function.array-rand.php)
- [Blog ProgramistaMłody.pl: Losowanie liczb w PHP](https://programistamlody.pl/php/losowanie-liczb-w-php)