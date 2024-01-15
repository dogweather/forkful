---
title:                "Generowanie losowych liczb"
html_title:           "Gleam: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb może być przydatne w wielu aplikacjach, takich jak gry, symulacje lub weryfikacji losowych wyborów. Ponadto, jest to ważna umiejętność w programowaniu, ponieważ pozwala na stworzenie bardziej złożonych i różnorodnych algorytmów.

## Jak to zrobić

```Gleam
// Generowanie losowej liczby całkowitej z przedziału 1 do 10
let random_num = Random.int(1, 10)

// Generowanie losowej liczby rzeczywistej z przedziału 0 do 1
let random_float = Random.float(0, 1)

// Generowanie losowego ciągu znaków o długości 10
let random_string = Random.string(10)
```

Output: 
```
random_num = 4
random_float = 0.735678
random_string = "aR82nJ4k9p"
```

## Głębszy zanurzenie

Generowanie losowych liczb jest możliwe dzięki tzw. generatorom pseudolosowym, które wykorzystują algorytm i ziarno, aby generować liczby, które wydają się być losowe. W Gleam, funkcje `int()`, `float()` i `string()` korzystają z generatora Mersenne Twister, który jest uważany za jeden z najbardziej wydajnych i dokładnych generatorów w świecie programowania.

Dodatkowo, w Gleam istnieją także funkcje `bool()` i `choose()` pozwalające na generowanie losowych wartości typu bool oraz wybór losowej wartości z listy podanych wartości.

## Zobacz również

- Dokumentacja Gleam na temat generowania liczb: https://gleam.run/documentation/std-lib-random/
- Sposoby na wprowadzenie losowości do kodu: https://www.freecodecamp.org/news/random-number-generator-in-python/ 
- Wyjaśnienie działania generatorów pseudolosowych: https://en.wikipedia.org/wiki/Pseudorandom_number_generator