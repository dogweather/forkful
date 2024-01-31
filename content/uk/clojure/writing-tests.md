---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Тестування коду – це перевірка, чи ваші функції ведуть себе як очікується. Програмісти тестують, щоб уникнути помилок, забезпечити якість та спростити подальші зміни коду.

## Як робити:
Тестування в Clojure зазвичай роблять із `clojure.test`. Ось приклад простого тесту:

```Clojure
(require '[clojure.test :refer :all])

(deftest test-sum
  (testing "Перевірка функції додавання"
    (is (= 10 (+ 5 5)))
    (is (= 7 (+ 3 4)))))
  
(run-tests)

;; Вивід
;;
;; Testing user
;;
;; Ran 1 tests containing 2 assertions.
;; 0 failures, 0 errors.
```

## Поглиблене вивчення:
Тести з'явились одночасно з першими програмами. Clojure використовує бібліотеку `clojure.test`, але є альтернативи як Midje чи test.check для властивостей тестування. Деталі реалізації включають виконання як блоків `testing`, так і окремих асерцій `is`.

## Дивіться також:
- [Clojure Testing with clojure.test](https://clojure.org/guides/deps_and_cli#_testing)
- [Introduction to Midje](https://github.com/marick/Midje/wiki)
- [clojure.test.check](https://github.com/clojure/test.check)
