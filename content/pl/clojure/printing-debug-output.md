---
title:                "Clojure: Wyświetlanie wyników debugowania"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyświetlanie informacji debugujących jest kluczowym narzędziem dla programistów w celu zrozumienia działania ich kodu i znajdowania błędów. Jest to niezbędne dla skutecznego rozwiązywania problemów w aplikacjach i ułatwia proces debugowania.

## Jak to zrobić

W Clojure istnieje kilka sposobów na wyświetlanie informacji debugujących. Jednym z najprostszych jest użycie funkcji `println`, która wypisze podane wartości do standardowego wyjścia.

```Clojure
(println "Hello World!")
```

Output:
```
Hello World!
```

Można również użyć funkcji `prn`, która wypisze podane wartości w formie drukowalnej. Jest to pomocne w przypadku wyświetlania złożonych struktur danych.

```Clojure
(prn {:name "John" :age 30})
```

Output:
```
{:name "John", :age 30}
```

Jeśli potrzebujemy wyświetlić informacje debugujące bezpośrednio w kodzie, możemy użyć makra `println` lub `prn`.

```Clojure
(defn add [x y]
  (println "Adding" x "and" y)
  (+ x y))
```

Output:
```
Adding 2 and 3
```

## Głębsza analiza

Ponieważ wyświetlanie informacji debugujących jest nieodłączną częścią procesu programowania, warto nauczyć się bardziej zaawansowanych technik w Clojure. Możemy na przykład użyć funkcji `str`, która pozwala na wyświetlenie różnych wartości w formie jednego stringa.

```Clojure
(str "The answer is" 42)
```

Output:
```
"The answer is 42"
```

Możemy również wypisać wartości w formacie CSV za pomocą funkcji `clojure.string/join`.

```Clojure
(require '[clojure.string :as str])
(def data [1 2 3])
(str/join "," data)
```

Output:
```
"1,2,3"
```

## Zobacz także

- [Dokumentacja Clojure](https://clojuredocs.org/)

- [10 sposobów na debuggerowanie w Clojure](https://lispcast.com/debugging-clojure/)

- [Jak pisać czytelny kod w Clojure](https://purelyfunctional.tv/guide/clojure-style-guide/)