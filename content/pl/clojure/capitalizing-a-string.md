---
title:                "Clojure: Zmiana wielkości litery napisu"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Niedawno natknąłem się na problem związany z koniecznością zmiany wielkości liter w ciągu znaków w moim programie Clojure. Musiałem wymusić, aby tylko pierwsza litera była wielka, a reszta małych liter. Zastanawiałem się, dlaczego ktoś w ogóle miałby się tego nauczyć? Cóż, istnieje wiele sytuacji, w których może być przydatne wykorzystanie tej funkcji, na przykład w celu poprawnej prezentacji danych lub wykorzystania jej w celu porównywania ciągów znaków.

## Jak

### Za pomocą wbudowanej funkcji

Clojure zapewnia nam wygodną wbudowaną funkcję o nazwie "capitalize", która zmienia pierwszą literę każdego słowa w ciągu znaków na wielką, a pozostałe na małe.

```Clojure
(capitalize "jednostka testowa")
```

Output: "Jednostka Testowa"

### Poprzez własną funkcję

Możemy również stworzyć własną funkcję, która będzie działać w podobny sposób. Najpierw musimy podzielić ciąg na pojedyncze słowa, a następnie zmienić pierwszą literę każdego słowa na wielką.

```Clojure
(defn capitalize-custom [string]
  (->> (clojure.string/split string #" ")
      (map clojure.string/capitalize)
      (apply str)))
```

Wywołanie:

```Clojure
(capitalize-custom "jednostka testowa")
```

Output: "Jednostka Testowa"

## Głębsza analiza

Warto zauważyć, że funkcje "capitalize" i "capitalize-custom" nie zmieniają wielkości liter w słowach, które już są napisane wielkimi literami. Na przykład, jeśli nasz ciąg znaków wygląda tak: "Prosta Funkcja", to wynikiem będzie "Prosta Funkcja", a nie "Prosta funkcja". 

Możemy jednak rozszerzyć naszą własną funkcję, aby również zmieniała wielkość liter w takich słowach. W tym celu możemy użyć funkcji "clojure.string/lower-case" w połączeniu z "clojure.string/capitalize" do zmiany wszystkich liter na małe, a następnie ponownie zmienić pierwszą literę na wielką.

```Clojure
(defn capitalize-custom [string]
  (->> (clojure.string/split string #" ")
    (map (fn [word] (clojure.string/capitalize (clojure.string/lower-case word))))
    (apply str)))
```

Wywołanie:

```Clojure
(capitalize-custom "Prosta Funkcja")
```

Output: "Prosta Funkcja"

## Zobacz również

- https://clojuredocs.org/clojure.string/lower-case
- https://clojuredocs.org/clojure.string/capitalize