---
title:                "Wyświetlanie wyników debugowania"
html_title:           "Clojure: Wyświetlanie wyników debugowania"
simple_title:         "Wyświetlanie wyników debugowania"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, debugowanie jest nieodłącznym elementem procesu tworzenia oprogramowania. Wyświetlanie informacji o działaniu programu może pomóc programistom w identyfikacji i rozwiązywaniu błędów w kodzie. Ten artykuł przedstawia metody wyświetlania informacji debugujących w języku Clojure, aby pomóc w zrozumieniu, dlaczego jest to ważne i jak tego dokonać.

## Jak To Zrobić

```Clojure
;; Użyj funkcji (println), aby wyświetlić tekst lub wartość wyrażenia w konsoli
(println "Hello World!")

;; Możesz również użyć funkcji (println) do wyświetlania wartości zmiennych
(def name "John")
(println "Imię użytkownika to:" name)

;; Użyj funkcji (pr) do wyświetlania sformatowanych informacji lub wyrażeń
(pr "Suma 2 i 3 to:" (+ 2 3))

;; Aby wyświetlić więcej niż jedną informację na raz, użyj funkcji (println) lub (prn)
(println "Jesteśmy w funkcji (println)!") (prn "A w (prn)...")
```

Przykładowy output:

```
Hello World!
Imię użytkownika to: John
Suma 2 i 3 to: 5
Jesteśmy w funkcji (println)!
A w (prn)...
```

## Deep Dive

W języku Clojure istnieje wiele funkcji, które można użyć do drukowania informacji debugujących. Oprócz funkcji (println) i (pr), istnieją także funkcje (pprint), (format), (printf), (print), (eprintln) i (doseq), aby wymienić tylko kilka przykładów. Każda z tych funkcji może być przydatna w różnych sytuacjach, dlatego warto zapoznać się z nimi i zastosować odpowiednią do potrzeb.

Dodatkowo, w celu lepszego wyświetlania informacji debugujących, można także używać bibliotek takich jak clj-logging i tools.logging, które oferują różne funkcje do formatowania i wyświetlania logów.

## Zobacz także

- [Dokumentacja "Clojure spec" dla funkcji (println)](https://clojure.org/reference/repl_and_main#_print_and_friends)
- [Biblioteka "clj-logging"](https://github.com/bsless/clj-logging)
- [Biblioteka "tools.logging"](https://github.com/clojure/tools.logging)