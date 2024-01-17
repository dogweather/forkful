---
title:                "Wydrukowanie wyników debugowania"
html_title:           "Clojure: Wydrukowanie wyników debugowania"
simple_title:         "Wydrukowanie wyników debugowania"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego: 
Wypisywanie danych debugowania to proces wypisywania informacji podczas działania programu w celu lepszej analizy i zrozumienia jego działania. Programiści często używają tej techniki, aby śledzić zmienne, wartości i inne parametry w celu znajdowania błędów i poprawiania ich kodu.

## Jak to zrobić:
Wypiszmy wartość zmiennej ```x``` w naszym programie, używając funkcji ```println```:
```Clojure
(def x 10)
(println x)
```
Wyjście powinno wyglądać tak:
```Clojure
10
```
Możemy również wyświetlić więcej danych, używając formatowania ciągu znaków. Na przykład:
```Clojure
(let [name "Jan" age 25]
  (println (str "Witaj, jestem " name " i mam " age " lat!")))
```
Wyjście:
```Clojure
Witaj, jestem Jan i mam 25 lat!
```
Jeśli potrzebujemy sprawdzić, czy dana funkcja jest wywoływana w naszym programie, możemy wypisać informacje o jej wywołaniu używając funkcji ```timbre/info```. Na przykład:
```Clojure
(defn moja-funkcja [parametr]
  (timbre/info (str "Funkcja moja-funkcja została wywołana z parametrem " parametr))
  ; kod funkcji
  )
```

## Głębsze wynurzenia:
Technika wypisywania danych debugowania jest popularna od dawna, gdy nie istniały jeszcze narzędzia debugowania w IDE. Alternatywnym sposobem może być użycie wbudowanego debuggera w Clojure REPL, który pozwala na interaktywne wykonywanie kodu i debugowanie błędów. Kod do wypisywania danych debugowania jest również przydatny przy testowaniu i monitorowaniu wydajności aplikacji w środowisku produkcyjnym. Implementacja funkcji ```println``` jest prosta, ponieważ jest to jedna z podstawowych funkcji języka Clojure. Można również używać innych funkcji wypisywania danych, takich jak ```print```, ```pr```, ```format``` lub ```str```.

## Zobacz też:
- [Clojure REPL Debugger](https://clojure.org/guides/repl/debugging)
- [Jak wykorzystać wypisywanie danych debugowania w Twoim kodzie Clojure](https://www.braveclojure.com/debugging/)
- [Timbre - biblioteka do wypisywania i logowania w Clojure](https://github.com/ptaoussanis/timbre)