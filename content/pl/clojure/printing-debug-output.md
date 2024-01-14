---
title:                "Clojure: Drukowanie wyników debugowania"
simple_title:         "Drukowanie wyników debugowania"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Nie ma nic bardziej frustrującego niż próba znalezienia błędu w swoim kodzie. Często musimy wyświetlić wartości zmiennych w różnych częściach funkcji, aby zrozumieć, co się dzieje. W takich sytuacjach bardzo przydatne jest umiejętne korzystanie z drukowania informacji debugowania.

## Jak to zrobić

Drukowanie informacji debugowania w Clojure jest bardzo proste. Wystarczy użyć funkcji ```println``` lub ```prn```, a następnie umieścić w niej wartości zmiennych, które chcemy wyświetlić. Na przykład:

```Clojure
(def x 5)
(def y 10)

(println "Wartość x to:" x)
(println "Wartość y to:" y)

; Output:
; Wartość x to: 5
; Wartość y to: 10
```

Możemy również użyć funkcji ```prn```, która wyświetli wartości w bardziej czytelny sposób:

```Clojure
(prn "Wartość x to:" x)
(prn "Wartość y to:" y)

; Output:
; "Wartość x to:" 5
; "Wartość y to:" 10
```

## Głębsze zagłębienie

Drukowanie informacji debugowania w Clojure może być również niezwykle pomocne podczas testowania kodu. Możemy umieścić drukowanie w różnych miejscach naszego kodu, aby śledzić wartości zmiennych w różnych punktach wykonania. Możemy też umieścić warunki, które będą wyświetlać informacje tylko wtedy, gdy spełnione zostaną odpowiednie warunki.

Na przykład, możemy wyświetlić wartości tylko wtedy, gdy jesteśmy w środowisku deweloperskim, a nie w produkcji:

```Clojure
(def environment "developers")

(if (= environment "developers")
  (println "Wartość x to:" x)
  (println "Jesteś w środowisku produkcji, więc nie możesz drukować wartości."))
```

Możemy również wyświetlić informację tylko wtedy, gdy wartość zmiennej przekracza pewną wartość:

```Clojure
(def x 20)

(if (> x 10)
  (prn "Wartość x przekroczyła 10!")
  (println "Wartość x jest mniejsza niż 10."))
``` 

## Zobacz także

- [Oficjalna dokumentacja Clojure](https://clojure.org/)
- [Blog Clojure](https://blog.clojure.org/)
- [Repozytorium Clojure na GitHubie](https://github.com/clojure)