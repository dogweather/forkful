---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych jest tworzeniem sekwencji liczb, które nie mają występującego wcześniej wzoru, są nieprzewidywalne. Programiści robią to dla wielu celów, takich jak tworzenie danych testowych, modelowanie i symulacje, gry, kryptografia i wiele innych aplikacji.

## Jak to zrobić:

Generowanie liczb losowych w Clojure jest proste dzięki wbudowanej funkcji `rand`. 

```Clojure
;; Generowanie losowej liczby
(rand)
;; => 0.6598789386091669
```

Jeśli chcemy wygenerować losową liczbę całkowitą z określonego zakresu, możemy skorzystać z funkcji `rand-int`.

```Clojure
;; Generowanie losowej liczby całkowitej pomiędzy 0 a 9
(rand-int 10)
;; => 7
```

## Glebokie zanurzenie

Generowanie liczb losowych to temat, który znacznie przekracza ramy tego artykułu, ale służy też jako ważne wprowadzenie do wielu dziedzin informatyki i matematyki. 

1. **Kontekst historyczny**: Historia generatorów liczb losowych sięga 1946 roku, kiedy to John von Neumann przedstawił „Metodę średniej kwadratowej”.
2. **Alternatywy**: Clojure ma kilka bibliotek zewnętrznych, które zapewniają zaawansowane funkcje generowania liczb losowych, takie jak [test.check](https://github.com/clojure/test.check) i [incanter](https://github.com/incanter/incanter).
3. **Szczegóły implementacji**: W Clojure, funkcje `rand` i `rand-int` używają generatora liczb losowych `java.util.Random` pod spodem. 

## Zobacz również

- [Dokumentacja Clojure na temat generowania liczb losowych](https://clojuredocs.org/clojure.core/rand)
- [java.util.Random](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html) - generator liczb losowych używany przez Clojure
- [Incanter](https://github.com/incanter/incanter) - Biblioteka do statystyki i generowania liczb losowych dla Clojure
- [test.check](https://github.com/clojure/test.check) - Biblioteka do generowania losowych danych testowych dla Clojure