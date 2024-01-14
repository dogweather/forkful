---
title:                "Clojure: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testowanie jest nieodłączną częścią pisania aplikacji w Clojure. Napisanie testów pomaga w upewnieniu się, że kod działa poprawnie i przewiduje możliwe błędy. Pomaga również w utrzymaniu kodu i łatwiejszym wprowadzaniu zmian w przyszłości. 

## Jak to zrobić

Aby stworzyć testy w Clojure, potrzebujesz użycia funkcji *deftest* oraz *is* z biblioteki *clojure.test*. Przykładowy kod poniżej będzie testował funkcję, która zwraca sumę dwóch liczb:

```Clojure
(deftest test-addition
  (is (= 8 (addition 3 5)))
  (is (= 11 (addition 6 5)))
)
```

Po uruchomieniu tych testów, jeśli funkcja *addition* działa poprawnie, otrzymamy wynik:

```Clojure
Testing user
Ran 2 tests containing 4 assertions.
0 failures, 0 errors.
=> {:testing 2, :success 2, :expected 4, :assert 4, :deftest 1}
```

To oznacza, że wszystkie asercje przeszły i nasza funkcja działa poprawnie.

## Deep Dive

Pisanie testów w Clojure może być skomplikowane, ponieważ wymaga użycia makr, aby sprawdzić czy asercje zostały spełnione. Dlatego ważne jest, aby zawsze korzystać z biblioteki *clojure.test*, która dostarcza narzędzia do przeprowadzania testów w sposób poprawny i czytelny.

Jednym z powodów, dla których warto pisać testy jest to, że pozwala to na łatwiejsze wprowadzanie zmian w kodzie w przyszłości. Ponadto, testy mogą służyć jako dokumentacja dla innych programistów, którzy będą pracować nad projektem.

## Zobacz również

- [Dokumentacja biblioteki clojure.test](https://clojure.github.io/clojure/clojure.test-api.html)
- [Livestream: Testowanie funkcji w Clojure](https://www.youtube.com/watch?v=RWlGnL7r_Xk)
- [Artykuł: "Testowanie w Clojure"](https://jaxenter.de/testen-mit-clojure-tutorial-34678)