---
title:    "Clojure: Generowanie losowych liczb"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest jednym z ważniejszych aspektów programowania w wielu językach, w tym również w Clojure. Ta umiejętność jest nie tylko niezbędna do tworzenia gier i symulacji, ale także do pracy z dużymi zestawami danych lub testowaniem algorytmów. W artykule tym dowiesz się, jak w prosty sposób generować losowe liczby w Clojure i wykorzystywać je w swoich projektach.

## Jak to zrobić

Clojure ma wbudowane funkcje, które pozwalają na generowanie losowych liczb w różnych formatach. Jednym z najprostszych sposobów jest użycie funkcji `rand-int`, która generuje losową liczbę całkowitą z podanego zakresu. Spróbujmy wygenerować 10 liczb od 1 do 100 i wyświetlić je w konsoli:

```Clojure
(dotimes [n 10]
  (println (rand-int 100)))
```

Output:

```
22
76
51
96
3
89
40
15
60
83
```

Możesz również wygenerować losową liczbę zmiennoprzecinkową używając funkcji `rand` w następujący sposób:

```Clojure
(rand)
```

Output:

```
0.437400299644851
```

Jeśli chcesz wygenerować liczbę zmiennoprzecinkową w określonym zakresie, możesz użyć funkcji `rand-nth`, która przyjmuje kolekcję liczb jako argument i zwraca losową liczbę z tej kolekcji. Na przykład, jeśli chcemy wygenerować losową liczbę z zakresu od 1 do 10, możemy użyć następującego kodu:

```Clojure
(rand-nth (range 1 11))
```

Output:

```
9
```

## Pełna analiza

Generowanie losowych liczb w Clojure opiera się na generatorach liczb pseudolosowych, które są funkcjami generującymi losowe liczby na podstawie stałych wartości zwanych ziarnem (ang. seed). Dzięki temu, przy każdym uruchomieniu programu, losowe liczby będą generowane w taki sam sposób, co ułatwia debugowanie i testowanie.

Clojure zapewnia również możliwość ustawienia własnego ziarna za pomocą funkcji `set!` i zwraca obecny stan ziarna za pomocą funkcji `get`. To umożliwia kontrolowanie wygenerowanych liczb i odtwarzanie wyników w przypadku potrzeby.

Możliwe jest również wykorzystanie funkcji `shuffle` do losowego ustawienia elementów w kolekcji lub `repeatedly` do wygenerowania określonej liczby losowych elementów z daną funkcją.

## Zobacz również

 - Oficjalna dokumentacja Clojure na temat generowania losowych liczb: https://clojure.org/guides/random_numbers
 - Przykładowe projekty wykorzystujące generowanie losowych liczb w Clojure: https://github.com/otann/random-source-clj, https://github.com/lorentzkim/clojure-random
 - Wprowadzenie do języka Clojure: https://scastie.scala-lang.org/3tyAsbNoQkyh1P9yUvRnYQ