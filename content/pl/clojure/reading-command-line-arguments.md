---
title:                "Clojure: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Dlaczego warto wczytać argumenty wiersza poleceń w Clojure

W dzisiejszych czasach programowanie jest nieodłączną częścią naszego życia. Wiele języków programowania wciąż jest wykorzystywanych w różnych dziedzinach - od tworzenia stron internetowych po analizę danych. Jednym z popularnych języków jest Clojure, który wyróżnia się swoją składnią i możliwościami programowania funkcyjnego. Jedną z przydatnych umiejętności w Clojure jest wczytywanie argumentów z linii poleceń. W tym wpisie dowiesz się, dlaczego warto to umiejętność zdobyć oraz jak to zrobić.

## Jak wczytać argumenty z linii poleceń w Clojure

Wczytywanie argumentów z linii poleceń jest przydatne w przypadku, gdy chcemy uruchamiać nasz program z różnymi parametrami. W Clojure możemy tego dokonać za pomocą funkcji *command-line-args* z biblioteki *clojure.java.shell*. Poniżej znajduje się przykładowy kod, który wczyta argumenty i wyświetli je na konsoli.

```Clojure
(ns args-example.core
  (:require [clojure.java.shell :refer [command-line-args]]))

(defn -main
  [args]
  (println "Wczytane argumenty:")
  (println args))

```

Kod ten wykorzystuje funkcję *command-line-args*, która zwraca listę wszystkich argumentów podanych podczas uruchamiania programu. Możemy także dokonać konwersji tych argumentów na odpowiednie typy danych, na przykład za pomocą funkcji *read-string*.

```Clojure
(ns args-example.core
  (:require [clojure.java.shell :refer [command-line-args]]
            [clojure.edn :as edn]))

(defn -main
  [args]
  (println "Wczytane argumenty:")
  (println args)
  (println "Pierwszy argument jako int:")
  (let [first-arg (read-string (first args))]
    (println (type first-arg))
    (println first-arg)))

```

Wydruk na konsoli dla argumentów "5" "hello":

```
Wczytane argumenty:
("5" "hello")
Pierwszy argument jako int:
clojure.lang.Long
5
```

## Głębszy wgląd w wczytywanie argumentów z linii poleceń

Warto wiedzieć, że funkcja *command-line-args* zwraca również informacje o środowisku, w którym uruchomiono program. Możemy to wykorzystać, aby wykonać odpowiednie akcje w zależności od tego, czy program został uruchomiony w środowisku produkcyjnym czy testowym. 

```Clojure
(ns args-example.core
  (:require [clojure.java.shell :refer [command-line-args]]
            [clojure.edn :as edn]))

(defn -main
  [args]
  (if (contains? (first args) :env)
    (println "Program został uruchomiony w środowisku testowym.")
    (println "Program został uruchomiony w środowisku produkcyjnym.")))

```

## Zobacz także

- Dokumentacja funkcji command-line-args w [ClojureDocs](https://clojuredocs.org/clojure.java.shell/command-line-args). 
- Wprowadzenie do programowania funkcyjnego w [Clojure na hackernoon.com](https://hackernoon.com/clojure-beginner-guide-advanced-concepts-callbacks-higher-order-fn-fb628c1f3e16).