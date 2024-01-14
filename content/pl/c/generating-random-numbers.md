---
title:    "C: Generowanie losowych liczb"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest nieodłączną częścią programowania w języku C. Może być wykorzystywane w różnych celach, takich jak testowanie algorytmów, symulacje lub tworzenie losowych wartości dla gier. Jest to przydatna funkcjonalność, której warto nauczyć się w celu ulepszenia swoich umiejętności programistycznych.

## Jak to zrobić

Aby wygenerować losowe liczby w języku C, musimy użyć funkcji `rand()`, która jest dostępna w standardowej bibliotece `stdlib.h`. Ta funkcja zwraca pseudo-losową liczbę z zakresu od 0 do `RAND_MAX`, który jest stałą zdefiniowaną w bibliotece. Aby wygenerować liczbę w określonym zakresie, możemy użyć prostej formuły:

```C
int losowa_liczba = rand() % zakres + początek;
```

Powyższy kod wygeneruje liczbę z zakresu od `początek` do `zakres + początek - 1`. Jeśli chcemy wygenerować liczbę zmiennoprzecinkową, możemy użyć funkcji `double random = (double)rand() / RAND_MAX;`, która zwróci wartość z przedziału od 0 do 1.

## Głębszy zanurzenie

Funkcja `rand()` jest oparta na tzw. generatorze liczb pseudolosowych (PRNG - ang. Pseudo-Random Number Generator), który używa określonego algorytmu do generowania sekwencji liczb, które wydają się być losowe. Jednak w rzeczywistości te liczby są ustalone i powtarzają się po pewnym czasie. Dlatego nie powinno się używać funkcji `rand()` do celów, które wymagają wyjątkowej nieprzewidywalności, takich jak generowanie haseł.

W języku C dostępna jest również funkcja `srand()`, która ustawia tzw. ziarno generatora, co pozwala na kontrolowane generowanie sekwencji liczb. Jeśli funkcja `srand()` nie jest wywoływana, domyślnie używana jest wartość `1` jako ziarno.

## Zobacz także

- Dokumentacja funkcji `rand()` w języku C: https://www.cplusplus.com/reference/cstdlib/rand/
- Wszystko o generatorach liczb pseudolosowych: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Przykładowe projekty wykorzystujące generowanie liczb losowych w języku C: https://www.codeproject.com/Articles/675417/Random-Number-Generators-RNGs-in-Cplusplus
- Dlaczego nie powinieneś używać `rand()` do generowania haseł: https://nullprogram.com/blog/2013/07/27/