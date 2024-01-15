---
title:                "Generowanie liczb losowych"
html_title:           "PHP: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest bardzo przydatne w programowaniu, szczególnie w sytuacjach, gdzie potrzebujemy losowych danych, takich jak generowanie losowych haseł, identyfikatorów lub testowania algorytmów. Jest to również popularna funkcja w grach i aplikacjach, które wymagają losowego zachowania.

## Jak to zrobić

 Istnieje wiele sposobów na generowanie losowych liczb w PHP. Jednym z najprostszych sposobów jest użycie funkcji `rand()`, która zwraca losową liczbę całkowitą w określonym zakresie. Przykładowy kod wykorzystujący tę funkcję wygląda następująco:

```PHP
$randomNumber = rand(1, 10); // zwraca losową liczbę z zakresu 1-10
echo $randomNumber; // wypisze np. 7
```

Możemy również wygenerować losowe liczby zmiennoprzecinkowe, używając funkcji `mt_rand()`, która jest szybsza i bardziej niezawodna. Przykładowy kod używający tej funkcji wygląda tak:

```PHP
$randomFloat = mt_rand() / mt_getrandmax(); // zwraca losową liczbę zmiennoprzecinkową z zakresu 0-1
echo $randomFloat; // wypisze np. 0.765821436
```

Możemy również wykorzystać funkcję `random_int()`, która jest jeszcze bardziej bezpieczna i zalecana do generowania losowych liczb wrażliwych na bezpieczeństwo, takich jak hasła. Przykładowy kod z użyciem tej funkcji wygląda następująco:

```PHP
$secureNumber = random_int(1000, 9999); // zwraca losową liczbę całkowitą z zakresu 1000-9999
echo $secureNumber; // wypisze np. 5683
```

Możemy również wygenerować losowe znaki, wykorzystując gotową funkcję `str_shuffle()`, która losowo modyfikuje podany ciąg znaków. Przykładowy kod wygląda tak:

```PHP
$characters = 'abcdefghijklmnopqrstuvwxyz'; // nasz alfabet
$shuffled = str_shuffle($characters); // losowo modyfikuje kolejność liter
echo $shuffled; // wypisze np. eqrpzmlsitjndxwocbhgaykfuv
```

Są oczywiście także inne sposoby na generowanie losowych liczb w PHP, ale te są najpopularniejsze i wystarczające dla większości projektów.

## Deep Dive

Funkcje `rand()` i `mt_rand()` korzystają z generatora liczb pseudolosowych, który generuje ciąg liczb na podstawie ziarna (seed). Z tego powodu, jeśli nie podamy ziarna ręcznie, każde wywołanie funkcji zwróci inne liczby. Dzięki temu możemy zapewnić, że wygenerowane liczby będą naprawdę losowe.

Funkcja `random_int()`, w przeciwieństwie do poprzednich dwóch, wykorzystuje generator liczb kryptograficznych, który jest jeszcze bardziej bezpieczny i zwraca liczby o wysokiej entropii. Wymaga ona jednak większych zasobów i może być wolniejsza w działaniu.

Możemy także zastosować funkcję `shuffle()` do losowego przemieszczenia elementów w tablicy. Jest to przydatne, jeśli chcemy losowo ułożyć elementy w celu losowego wyboru lub generowania losowych kombinacji.

## Zobacz także

- Dokumentacja PHP dla funkcji rand(): https://www.php.net/manual/en/function.rand.php
- Dokumentacja PHP dla funkcji mt_rand(): https://www.php.net/manual/en/function.mt-rand.php
- Dokumentacja PHP dla funkcji random_int(): https://www.php.net/manual/en/function.random-int.php