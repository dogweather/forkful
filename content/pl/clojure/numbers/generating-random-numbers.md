---
date: 2024-01-27 20:33:08.803763-07:00
description: "Generowanie liczb losowych w programowaniu polega na tworzeniu warto\u015B\
  ci, kt\xF3rych nie mo\u017Cna logicznie przewidzie\u0107 z g\xF3ry. Programi\u015B\
  ci robi\u0105 to z r\xF3\u017Cnych\u2026"
lastmod: '2024-03-13T22:44:34.990425-06:00'
model: gpt-4-0125-preview
summary: "Generowanie liczb losowych w programowaniu polega na tworzeniu warto\u015B\
  ci, kt\xF3rych nie mo\u017Cna logicznie przewidzie\u0107 z g\xF3ry."
title: Generowanie liczb losowych
weight: 12
---

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
