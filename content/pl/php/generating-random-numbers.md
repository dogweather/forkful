---
title:                "Tworzenie losowych liczb"
html_title:           "PHP: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Generowanie losowych liczb jest procesem polegającym na wygenerowaniu liczby całkowitej lub rzeczywistej w sposób losowy. Programiści często stosują tę technikę do symulacji losowych zdarzeń lub do generowania unikalnych identyfikatorów.

# Jak to zrobić?

W PHP możemy wykorzystać funkcję `rand()` do generowania losowych liczb całkowitych w określonym zakresie. Przykładowy kod wygląda następująco:
```
$x = rand(1, 10);
echo $x;
```
Wynik zostanie wyświetlony jako liczba całkowita z zakresu od 1 do 10.

Aby generować losowe liczby rzeczywiste, możemy użyć funkcji `mt_rand()`, która korzysta z algorytmu Mersenne Twister. Przykładowy kod dla wygenerowania liczby zmiennoprzecinkowej z zakresu od 0 do 1 wygląda tak:
```
$x = mt_rand() / mt_getrandmax();
echo $x;
```

# Wnikliwy przegląd

Generowanie losowych liczb jest nieodłączną częścią wielu programów i aplikacji. Początkowo, programiści korzystali z funkcji `rand()` w PHP, która wykorzystywała generator liczb pseudolosowych. Jednakże, dla zadań wymagających większej losowości, zaleca się używanie funkcji `mt_rand()`, która jest wydajniejsza i zapewnia lepsze rozkłady losowych wartości.

Alternatywą dla wewnętrznych funkcji PHP jest wykorzystanie zewnętrznych bibliotek, takich jak RandomLib czy PhpSecLib, które oferują bardziej zaawansowane opcje generowania losowych liczb.

Ważnym aspektem generowania losowych liczb jest ziarno (ang. seed), które jest używane do inicjalizacji generatora liczb pseudolosowych i określenia początkowego punktu wyjścia. W przypadku PHP, ziarno jest wybierane automatycznie, ale można je także ustalić ręcznie za pomocą funkcji `srand()`.

# Zobacz też

- Dokumentacja PHP dotycząca funkcji `rand()` i `mt_rand()`: https://www.php.net/manual/en/function.srand.php
- Biblioteka RandomLib: https://github.com/ircmaxell/RandomLib
- Biblioteka PhpSecLib: https://github.com/phpseclib/phpseclib