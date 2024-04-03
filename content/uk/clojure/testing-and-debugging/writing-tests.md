---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:15.169211-07:00
description: "\u042F\u043A: Clojure, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u044E\u0447\u0438 JVM, \u043F\u0456\u0434\u0442\u0440\u0438\u043C\
  \u0443\u0454 \u0440\u0456\u0437\u043D\u0456 \u0444\u0440\u0435\u0439\u043C\u0432\
  \u043E\u0440\u043A\u0438 \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\
  \u0430\u043D\u043D\u044F. \u041E\u0434\u043D\u0430\u043A, \u0437\u0430\u0433\u0430\
  \u043B\u044C\u043D\u043E\u043F\u0440\u0438\u0439\u043D\u044F\u0442\u043E\u044E \u0432\
  \u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u044E \u0431\u0456\u0431\u043B\u0456\
  \u043E\u0442\u0435\u043A\u043E\u044E \u0454 `clojure.test`. \u041E\u0441\u044C \u043F\
  \u0440\u043E\u0441\u0442\u0438\u0439\u2026"
lastmod: '2024-03-13T22:44:48.658955-06:00'
model: gpt-4-0125-preview
summary: "Clojure, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u044E\u0447\u0438 JVM, \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u0443\u0454 \u0440\
  \u0456\u0437\u043D\u0456 \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0438\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  ."
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

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
