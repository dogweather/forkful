---
title:    "Clojure: Drukowanie informacji debugowania"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

W programowaniu bardzo często wykorzystujemy funkcję drukowania informacji, zwanej również wyjściem debugowania. Pozwala nam to na zrozumienie i śledzenie działania naszego kodu, a także naprawianie błędów. Pisanie dobrej jakości kodu wymaga umiejętności korzystania z debugowania w celu zapewnienia jego poprawnego działania. 

## Jak to zrobić

Przykładowe wywołanie w języku Clojure do drukowania informacji:

```Clojure
(println "To jest przykładowa informacja debugowania!")
```

Przykładowy wynik:

```Clojure
To jest przykładowa informacja debugowania!
```

Możemy także wykorzystać makra (ang. macros) do drukowania informacji z dodatkowymi szczegółami, takimi jak numer linii kodu, w którym zostało wywołane lub wartość zmiennej. Przykładowe użycie makra `cider-debug/pprint`:

```Clojure
(def x 10)
(cider-debug/pprint "Wartość zmiennej x to" x)
```

Przykładowy wynik:

```Clojure
Wartość zmiennej x to 10
```

## Głębsze zagadnienia

W Clojure mamy wiele różnych opcji do drukowania informacji debugowania, w tym także użycie funkcji `clojure.pprint` do precyzyjnego formatowania wyjścia. Możemy także wykorzystać bibliotekę [spyscope](https://github.com/dgrnbrg/spyscope) do drukowania informacji z poziomu interaktywnego środowiska programistycznego.

## Zobacz także

- [Dlaczego warto używać funkcji `println` w Clojure](https://stackoverflow.com/questions/12097174/why-println-works-in-clojure-code)
- [Przykłady wykorzystania makra `cider-debug/pprint`](https://practicalli.github.io/clojure/development/ss-deprecated-libs/cider-debug/pprint.html)
- [Dokumentacja funkcji `clojure.pprint`](https://clojure.github.io/clojure/clojure.pprint-api.html)