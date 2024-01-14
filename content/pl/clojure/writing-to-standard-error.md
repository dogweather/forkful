---
title:                "Clojure: Pisanie do standardowego błędu"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego pisać do standardowego błędu?

Pisanie do standardowego błędu jest niezbędne w celu poprawnego debugowania i przechwytywania błędów w naszych programach Clojure. Dzięki temu możemy szybko zlokalizować przyczynę problemu i odpowiednio zareagować.

## Jak to zrobić?

```Clojure
(println "To jest standardowy błąd!")
```

Wyżej przedstawiony kod pokazuje, jak prostym sposobem możemy wypisać treść do standardowego błędu. Możemy również użyć funkcji `eprintln`, aby wypisać błąd wraz z jego treścią.

```
Standardowy błąd: To jest standardowy błąd!
```

## Głębszy wgląd

Pisanie do standardowego błędu jest szczególnie przydatne, gdy chcemy zobaczyć, który kod został wykonany przed błędem. Możemy to zrobić, wykorzystując funkcję `debug`, która wypisze nam stos wywołań dla danej funkcji.

```Clojure
(defn add [a b]
  (println "Jestem w funkcji add!")
  (+ a b))

(def x 10)
(def y "Test")

(add x y)
```

```
Standardowy błąd: Jestem w funkcji add!
Jestem w funkcji add!
TypeError: Cannot cast java.lang.String to java.lang.Number
```

Jak widać, dzięki wypisaniu do standardowego błędu, widzimy, że błąd wystąpił w funkcji "add" oraz mamy dokładną informację, jakie wartości zostały przekazane do funkcji.

## Zobacz też

- [Dokumentacja Clojure o pisanie do standardowego błędu](https://clojure.org/reference/exceptions#writing_to_standard_error)
- [Blog o programowaniu w Clojure (w języku polskim)](https://clojure.pl/blog/)