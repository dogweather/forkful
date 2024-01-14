---
title:                "Clojure: Pisanie testów"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Testowanie jest nieodłącznym elementem procesu programowania. Pisząc testy, możemy upewnić się, że nasz kod działa zgodnie z oczekiwaniami i uniknąć błędów, co przekłada się na lepszą jakość naszych aplikacji. W ten sposób także poprawiamy swoje umiejętności pisania kodu, ponieważ testowanie wymaga od nas myślenia w sposób bardziej zorganizowany i dokładny.

## Jak to zrobić

Aby napisać testy w Clojure, możemy skorzystać z biblioteki [clojure.test](https://clojuredocs.org/clojure.test). Przykładowy kod testujący wyglądałby tak:

```Clojure
(ns example.core-test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest add-numbers-test
  (is (= 4 (add-numbers 2 2))))

(deftest multiply-numbers-test
  (is (= 10 (multiply-numbers 5 2))))

(run-tests)

```

Powyższy kod testuje funkcje `add-numbers` i `multiply-numbers`, sprawdzając czy zwracają one poprawne wyniki dla podanych argumentów. Po uruchomieniu testów otrzymamy informację, czy testy przeszły pomyślnie, czy też nie. Jeśli któryś z testów nie przejdzie, oznacza to, że nasza funkcja nie działa poprawnie i musimy ją poprawić.

## Głębszy zanurzenie

Pisanie testów w Clojure polega głównie na wykorzystywaniu makr dostępnych w bibliotece `clojure.test`, takich jak `deftest` czy `is`. Możemy także tworzyć własne makra do testowania naszych funkcji, co pozwala nam na większą elastyczność w sposobie tworzenia testów.

Warto także pamiętać o tzw. "mockowaniu" (ang. mocking), czyli symulowaniu pewnych elementów naszego kodu w celu przetestowania innych funkcji. Biblioteka [midje](https://github.com/marick/Midje) jest przykładem narzędzia, które ułatwia nam to zadanie.

## Zobacz również

- [Oficjalna dokumentacja Clojure](https://clojure.org/)
- [Clojure docs - test library](https://clojuredocs.org/clojure.test)
- [Clojure for the Brave and True - rozdział o testowaniu](https://www.braveclojure.com/testing/)