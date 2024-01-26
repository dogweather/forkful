---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:07.857683-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Generowanie losowych liczb to proces tworzenia numerów, które nie mają z góry określonej kolejności i nie można ich przewidzieć. Programiści używają tych liczb w symulacjach, grach, testowaniu oprogramowania, i wszędzie tam, gdzie potrzebują elementu nieprzewidywalności.

## How to: (Jak to zrobić:)
Clojure używa namespace `java.util.Random` albo `clojure.core/rand` do generowania losowych liczb. Oto kilka przykładów:

```Clojure
;; Użycie java.util.Random dla liczb całkowitych
(import 'java.util.Random)
(let [rng (Random.)]
  (.nextInt rng)) ;=> losowa liczba całkowita

;; Użycie clojure.core/rand dla liczby zmiennoprzecinkowej od 0 do 1
(rand) ;=> 0.7099110737135088

;; Losowy element z kolekcji
(rand-nth [1 2 3 4 5]) ;=> 3

;; Losowa liczba całkowita w danym zakresie, np. od 0 do 99
(rand-int 100) ;=> 42
```

## Deep Dive (Głębokie zanurzenie)
Tradycyjnie, losowe liczby generowano za pomocą fizycznych metod, jak rzuty kostką czy losowania. W komputerach losowość jest symulowana – nazywamy to liczbami pseudolosowymi. Algorytmy takie jak algorytm Mersenne Twister lub Linear congruential generator (LCG) używane są do ich generowania.

W Clojure, `rand`, `rand-int`, i `rand-nth` opierają się na java.util.Random, który jest implementacją LCG. Istnieją lepsze generatory, jak `/dev/random` w systemach Unix czy SecureRandom w Javie, które używają różnych źródeł entropii, są wolniejsze, ale za to oferują wyższy stopień losowości.

Alternatywą dla `rand` mogą być biblioteki zewnętrzne jak `clj-random`, które mogą oferować większą kontrolę i dodatkowe funkcje.

## See Also (Zobacz również)
- [Clojure - Random](https://clojuredocs.org/clojure.core/rand)
- [Java.util.Random](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Random.html)
- [SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
