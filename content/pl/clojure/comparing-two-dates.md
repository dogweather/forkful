---
title:    "Clojure: Porównywanie dwóch dat"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest nieodłączną częścią wielu projektów programistycznych. Bardzo często musimy sprawdzić, czy jedna data jest wcześniejsza lub późniejsza od drugiej. W tym artykule dowiesz się, jak prawidłowo porównywać daty w języku Clojure.

## Jak to zrobić

Pierwszym krokiem jest utworzenie dwóch obiektów daty, które chcemy porównać. Możemy to zrobić za pomocą funkcji `clj-time.core/date-time`. Na przykład, chcemy porównać daty 1 stycznia 2020 i 1 lutego 2020, możemy to zrobić w następujący sposób:

```Clojure
(let [data1 (clj-time.core/date-time 2020 1 1)
      data2 (clj-time.core/date-time 2020 2 1)])
  ; tutaj będziemy porównywać daty
)
```

Kolejnym krokiem jest wykorzystanie funkcji `clj-time.core/before?` lub `clj-time.core/after?`, aby sprawdzić, czy jedna data jest wcześniejsza lub późniejsza od drugiej. Przykładowo, jeśli chcemy sprawdzić, czy data1 jest wcześniejsza niż data2, możemy użyć poniższego kodu:

```Clojure
(clj-time.core/before? data1 data2) ; zwróci true
```
Możemy również porównać daty z wykorzystaniem operatorów porównania `<` lub `>`, korzystając z funkcji konwersji `clj-time.core/to-long`:

```Clojure
(< (clj-time.core/to-long data1) (clj-time.core/to-long data2)) ; zwróci true
```

## Głębszy wgląd

Podczas porównywania dat, możemy również uwzględnić czas i strefę czasową. W takim przypadku, zaleca się użycie funkcji `clj-time.core/after?`, `clj-time.core/before?` lub operatorów porównania `>` lub `<`, wykorzystując funkcje konwersji `clj-time.core/to-instant` i `clj-time.core/to-long`. Przykładowo:

```Clojure
(let [data1 (clj-time.core/date-time 2020 1 1 10 30)
      data2 (clj-time.core/date-time 2020 1 1 9)
      strefa-czasowa (java.time.ZoneId/of "Europe/Warsaw")]
  (clj-time.core/after? (clj-time.core/to-instant (clj-time.core/to-long data1) strefa-czasowa)
                        (clj-time.core/to-instant (clj-time.core/to-long data2) strefa-czasowa))
)
; zwróci true
```

## Zobacz również

- [Dokumentacja clj-time](https://github.com/clj-time/clj-time)
- [Porównywanie dat w Clojure](https://medium.com/@practicalli/date-time-manipulation-using-clj-time-7241a87ea418)
- [Kurs Clojure dla początkujących](https://practicalli.github.io/clojure/)