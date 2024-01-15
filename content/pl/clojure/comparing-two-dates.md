---
title:                "Porównywanie dwóch dat"
html_title:           "Clojure: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś chciałby porównywać dwie daty? To proste. Porównywanie dat jest niezwykle przydatną umiejętnością w programowaniu, ponieważ pozwala nam na określenie kolejności zdarzeń, wykrywanie zależności czasowych oraz przetwarzanie i analizowanie danych historycznych.

## Jak to zrobić?

Porównywanie dwóch dat w Clojure jest łatwe i intuicyjne. Najprostszym sposobem jest użycie funkcji `compare`, która zwraca wartość liczbą całkowitą w zależności od wyniku porównania. Przykładowe użycie tej funkcji wyglądałoby tak:

```Clojure
(compare #inst "2021-04-15" #inst "2021-05-01")
```

W powyższym przykładzie porównujemy datę 15 kwietnia 2021 z datą 1 maja 2021. Wynik porównania będzie liczbą ujemną, ponieważ pierwsza data jest wcześniejsza niż druga.

Możemy również użyć funkcji `after?`, `before?` lub `between?` do bardziej zaawansowanych porównań. Przykład użycia `before?` wyglądałby tak:

```Clojure
(before? #inst "2021-04-15" #inst "2021-05-01")
```

W tym przypadku funkcja zwróci wartość `true`, ponieważ data 15 kwietnia 2021 jest wcześniejsza niż data 1 maja 2021.

## Głębsze zanurzenie

Porównywanie dat może być trochę bardziej skomplikowane, jeśli chcemy wziąć pod uwagę również inne elementy, takie jak godziny, minuty czy sekundy. W takim przypadku należy użyć funkcji `between?` w połączeniu z funkcją `chrono-range`, która pozwala na zdefiniowanie zakresu porównania w bardziej szczegółowy sposób.

```Clojure
(between? #inst "2021-04-15" #inst "2021-04-16" (chrono-range :hours))
```

W powyższym przykładzie określamy, że chcemy porównać dwie daty z dokładnością do godzin. Funkcja `between?` zwróci w tym przypadku wartość `true`, ponieważ obejmuje cały zakres od 15 kwietnia do 16 kwietnia.

Dzięki funkcjom jak `compare`, `after?`, `before?` i `between?` porównywanie dat w Clojure jest szybkie i proste. Warto również zapoznać się z innymi funkcjami z biblioteki `clj-time`, która oferuje szereg narzędzi do pracy z datami i czasem.

## Zobacz też

- [Dokumentacja Clojure - porównywanie dat](https://clojuredocs.org/clojure.core/compare)
- [Dokumentacja Clojure - biblioteka clj-time](https://clj-time.github.io/clj-time/)