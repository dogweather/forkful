---
title:    "Clojure: Pisanie do standardowego błędu"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Napisanie do standardowego wyjścia błędu jest ważnym aspektem programowania w Clojure, ponieważ pozwala na wyświetlenie informacji o błędach i wyjątkach, które mogą wystąpić w naszym kodzie. Jest to szczególnie przydatne podczas debugowania i znajdowania błędów w programie.

## Jak to zrobić

Aby napisać do standardowego wyjścia błędu w Clojure, możemy użyć funkcji `println` z argumentem `*err*`. Spowoduje to wyświetlenie podanego ciągu znaków w konsoli jako błąd. Na przykład:

```Clojure
(println *err* "Wystąpił błąd!")
```

Wyjście będzie wyglądać następująco:
```
Wystąpił błąd!
```

Możemy także wykorzystać makro `with-out-str`, aby prześledzić wszystkie informacje wyświetlane do standardowego wyjścia błędu. Na przykład:

```Clojure
(with-out-str (println *err* "Wystąpił błąd!"))
```

W ten sposób zadziała `println`, ale informacje nie zostaną faktycznie wyświetlone w konsoli. Zamiast tego, będą dostępne w ciągu znaków, który możemy przetworzyć lub zapisać do pliku.

## Deep Dive

Poza wyświetlaniem prostych komunikatów błędów, możemy także zwrócić obiekty typu `Exception`, w których możemy zawrzeć dokładniejsze informacje o błędzie. Na przykład:

```Clojure
(def error (ex-info "Ten błąd jest spowodowany przez brakujący argument!" {:missing-arg "nazwa argumentu"}))
```

Następnie, możemy wyświetlić ten błąd używając funkcji `prn` na obiekcie `error`:

```Clojure
(prn error)
```

Wyjście będzie wyglądać następująco:
```
{:cause "Ten błąd jest spowodowany przez brakujący argument!", :data {:missing-arg "nazwa argumentu"}}
```

Możemy także wykorzystać funkcję `throw` aby rzucić ten błąd w kodzie.

## Zobacz także

Aby uzyskać więcej informacji na temat pisania do standardowego wyjścia błędu w Clojure, warto zapoznać się z poniższymi linkami:

- Oficjalna dokumentacja Clojure: [https://clojuredocs.org/clojure.core/*err*](https://clojuredocs.org/clojure.core/*err*)
- Poradnik z przykładami: [https://purelyfunctional.tv/guide/error-output/](https://purelyfunctional.tv/guide/error-output/)
- Forum Clojure: [https://clojureverse.org/t/writing-to-standard-error/1596](https://clojureverse.org/t/writing-to-standard-error/1596)