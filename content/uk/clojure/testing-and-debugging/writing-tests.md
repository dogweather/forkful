---
title:                "Письмо тестів"
aliases: - /uk/clojure/writing-tests.md
date:                  2024-02-03T19:30:15.169211-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Написання тестів на Clojure, як і в інших мовах програмування, передбачає створення окремого коду для перевірки того, що ваш основний код працює очікуваним чином. Це допомагає забезпечити правильність, полегшує рефакторинг та підвищує стабільність коду.

## Як:
Clojure, використовуючи JVM, підтримує різні фреймворки для тестування. Однак, загальноприйнятою вбудованою бібліотекою є `clojure.test`. Ось простий приклад:

```clojure
(ns example.test
  (:require [clojure.test :refer :all]
            [example.core :refer :all]))

(deftest test-addition
  (testing "Функціонал додавання"
    (is (= 4 (add 2 2)))
    (is (= 7 (add 3 4)))))

(run-tests)
```
Після запуску цього тесту ви побачите результат, схожий на:

```
Testing example.test

Ran 2 tests containing 2 assertions.
0 failures, 0 errors.
```

Для тих, хто шукає більш функціональні варіанти, можна використовувати сторонні бібліотеки, такі як `Midje` або `test.check`. Ось як ви могли б використати Midje для подібного тесту:

Спочатку додайте Midje до залежностей вашого project.clj:
```clojure
[midje "1.9.9"]
```

Потім ваш тест з Midje може виглядати так:

```clojure
(ns example.test
  (:require [midje.sweet :refer :all]
            [example.core :refer :all]))

(fact "Тестування додавання"
  (add 2 2) => 4
  (add 3 4) => 7)
```

Після запуску тесту через Midje з `lein midje`, вивід покаже щось на зразок:

```
All checks (2) succeeded.
```
