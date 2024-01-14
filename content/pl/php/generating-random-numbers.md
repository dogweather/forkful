---
title:    "PHP: Generowanie losowych liczb"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest jednym z najważniejszych aspektów programowania w PHP. Wiele aplikacji, a zwłaszcza gier, wykorzystuje losowe liczby w celu zapewnienia zmienności i różnorodności w swoim działaniu. Dlatego warto zrozumieć, w jaki sposób generować losowe liczby w PHP.

## Jak to zrobić

Proces generowania losowych liczb w PHP jest prosty i dostępny dzięki wykorzystaniu wbudowanej funkcji `rand()`. Poniżej znajdziesz przykład kodu, który wygeneruje 10 losowych liczb z zakresu od 1 do 100:

```
<?php
for ($i=0; $i<10; $i++) {
  // wygenerowanie losowej liczby
  $randomNumber = rand(1, 100);
  // wyświetlenie jej na ekranie
  echo $randomNumber."\n";
}
?>
```

Powyższy kod wyświetli na ekranie coś w stylu:

```
83
15
47
96
40
77
32
99
61
8
```

W ten sam sposób można generować losowe liczby z innego zakresu lub zmiennego rozmiaru.

## Głębsze zanurzenie

Funkcja `rand()` korzysta z algorytmu pseudo-losowego generowania liczb, co oznacza, że jest to jednakowy proces, który kończy się zawsze tą samą liczbą w stałym odstępie. W celu generowania bardziej losowych liczb, można skorzystać z innych dostępnych funkcji, takich jak `mt_rand()`, `random_int()` czy `openssl_random_pseudo_bytes()`, które korzystają z lepszych algorytmów i zapewniają większą losowość.

## Zobacz również

- [Dokumentacja PHP do funkcji rand()](https://www.php.net/manual/en/function.rand.php)
- [Inne sposoby na generowanie losowych liczb w PHP](https://code.tutsplus.com/pl/tutorials/generating-random-numbers-in-php--cms-32136)
- [Wykorzystanie losowych liczb w aplikacjach w PHP](https://www.geeksforgeeks.org/generating-random-numbers-in-php/)