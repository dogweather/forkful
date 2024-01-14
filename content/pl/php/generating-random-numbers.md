---
title:    "PHP: Generowanie losowych liczb"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią wielu programów i aplikacji. Jeśli jesteś programistą PHP, na pewno potrzebujesz umiejętności generowania liczb losowych, aby tworzyć efektywny kod. W tym artykule pokażemy Ci, dlaczego warto brać pod uwagę generowanie losowych liczb w swoich projektach.

## Jak to zrobić

Aby wygenerować losową liczbę w PHP, musisz użyć funkcji `rand()` lub `mt_rand()`. Oba te funkcje zwracają losową liczbę w podanym zakresie. Oto przykład użycia `rand()`:

```PHP
<?php
$random_number = rand(1, 100);
echo "Wylosowana liczba to: " . $random_number;
// Wyświetli coś w stylu: Wylosowana liczba to: 67
```

Aby wygenerować losową liczbę zmiennoprzecinkową, możesz użyć funkcji `mt_rand()` w połączeniu z funkcją `mt_getrandmax()` w następujący sposób:

```PHP
<?php
$random_number = mt_rand() / mt_getrandmax();
echo "Wylosowana liczba zmiennoprzecinkowa: " . $random_number;
// Wyświetli coś w stylu: Wylosowana liczba zmiennoprzecinkowa: 0.8234
```

Możesz także użyć funkcji `shuffle()` do przetasowania elementów w tablicy lub `array_rand()` do wybrania losowego elementu z tablicy. Sprawdź dokumentację PHP, aby dowiedzieć się więcej o tych funkcjach.

## Deep Dive

Generowanie liczb losowych jest bardzo ważną częścią wielu algorytmów i systemów kryptograficznych. Funkcje `rand()` i `mt_rand()` są oparte na algorytmie Mersenne Twister, który jest powszechnie uważany za jeden z najlepszych algorytmów generowania liczb losowych. Warto pamiętać, że nie są one jednak całkowicie losowe i mogą być przewidywane w pewnym stopniu.

Jeśli potrzebujesz bardziej zaawansowanych metod generowania liczb losowych, możesz użyć rozszerzenia PHP o nazwie "Random Number Generation" (RNG). Wykorzystuje ono bardziej skomplikowany i nieprzewidywalny algorytm, który jest przydatny w przypadku aplikacji wymagających wysokiego poziomu bezpieczeństwa.

## Zobacz także

- [Dokumentacja PHP: Funkcja rand()](https://www.php.net/manual/pl/function.rand.php)
- [Dokumentacja PHP: Funkcja mt_rand()](https://www.php.net/manual/pl/function.mt-rand.php)
- [Dokumentacja PHP: Funkcja shuffle()](https://www.php.net/manual/pl/function.shuffle.php)
- [Dokumentacja PHP: Funkcja array_rand()](https://www.php.net/manual/pl/function.array-rand.php)
- [Rozszerzenie PHP: RNG](https://www.php.net/manual/pl/class.rng.php)