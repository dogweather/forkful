---
title:                "Generowanie liczb losowych"
date:                  2024-01-27T20:33:08.803763-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych w programowaniu polega na tworzeniu wartości, których nie można logicznie przewidzieć z góry. Programiści robią to z różnych powodów, w tym generowania unikalnych identyfikatorów, symulowania scenariuszy w rozwoju gier czy wybierania losowych próbek z danych do analizy.

## Jak to zrobić:

W Clojure generowanie liczb losowych jest proste, i istnieje kilka wbudowanych funkcji, które można od razu wykorzystać.

Aby wygenerować losową liczbę zmiennoprzecinkową pomiędzy 0 (włącznie) a 1 (wyłącznie), można użyć funkcji `rand`:

```Clojure
(rand)
;; Przykładowy wynik: 0.7094245047062917
```

Jeśli potrzebujesz liczby całkowitej w określonym zakresie, użyj `rand-int`:

```Clojure
(rand-int 10)
;; Przykładowy wynik: 7
```

To da ci losową liczbę całkowitą między 0 (włącznie) a liczbą, którą przekażesz jako argument (wyłącznie).

Aby wygenerować losową liczbę w określonym zakresie (nie ograniczając się do liczb całkowitych), możesz połączyć `rand` z arytmetyką:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; Użycie
(rand-range 10 20)
;; Przykładowy wynik: 14.857457734992847
```

Ta funkcja `rand-range` zwróci losową liczbę zmiennoprzecinkową pomiędzy wartościami `min` i `max`, które określisz.

W scenariuszach wymagających bardziej skomplikowanych rozkładów lub sekwencji liczb losowych, gdzie powtarzalność jest konieczna (użycie ziaren), możesz potrzebować zajrzeć do dodatkowych bibliotek, które oferują więcej niż to, co jest wbudowane.

## Zagłębienie się

Podstawowy mechanizm generowania liczb losowych w większości języków programowania, w tym w Clojure, zazwyczaj polega na użyciu generatora liczb pseudolosowych (PRNG). PRNG używa algorytmu do wytwarzania sekwencji liczb, które przybliżają właściwości liczb losowych. Warto zauważyć, że ponieważ są one generowane algorytmicznie, nie są one prawdziwie losowe, ale mogą być wystarczające dla większości praktycznych zastosowań.

W pierwszych dniach komputeryzacji generowanie wysokiej jakości liczb losowych było znaczącym wyzwaniem, co doprowadziło do rozwoju różnych algorytmów mających na celu poprawę losowości i dystrybucji. Dla Clojure wbudowane funkcje, takie jak `rand` i `rand-int`, są wygodne do codziennego użytku i obejmują szeroki zakres typowych przypadków użycia.

Jednak w przypadku aplikacji wymagających bezpieczeństwa kryptograficznego lub bardziej skomplikowanych metod próbkowania statystycznego, programiści Clojure często zwracają się ku zewnętrznym bibliotekom, które oferują bardziej zaawansowane i specjalistyczne PRNG. Biblioteki takie jak `clj-random` zapewniają dostęp do szerszego wachlarza algorytmów i większej kontroli nad ziarnami, co może być kluczowe dla symulacji, aplikacji kryptograficznych lub każdej dziedziny, gdzie jakość i przewidywalność sekwencji liczb losowych mogłaby mieć znaczące implikacje.

Choć wbudowane możliwości Clojure do generowania liczb losowych są wystarczające dla wielu zadań, eksploracja zewnętrznych bibliotek może zaoferować głębsze wglądy i opcje dla dostosowanych lub bardziej krytycznych zastosowań.
