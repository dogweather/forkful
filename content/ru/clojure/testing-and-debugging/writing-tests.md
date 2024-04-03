---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:43.408399-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Clojure \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 \u043F\u043E\u0434\
  \ \u043D\u0430\u0437\u0432\u0430\u043D\u0438\u0435\u043C `clojure.test` \u0434\u043B\
  \u044F \u043D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u044F \u0438 \u0437\u0430\
  \u043F\u0443\u0441\u043A\u0430 \u0442\u0435\u0441\u0442\u043E\u0432. \u0412\u043E\
  \u0442 \u043A\u0430\u043A \u044D\u0442\u043E \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u044C."
lastmod: '2024-03-13T22:44:44.356275-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 \u043F\u043E\u0434 \u043D\
  \u0430\u0437\u0432\u0430\u043D\u0438\u0435\u043C `clojure.test` \u0434\u043B\u044F\
  \ \u043D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u044F \u0438 \u0437\u0430\u043F\
  \u0443\u0441\u043A\u0430 \u0442\u0435\u0441\u0442\u043E\u0432."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

## Как это сделать:
Clojure использует библиотеку под названием `clojure.test` для написания и запуска тестов. Вот как это использовать:

```Clojure
(require '[clojure.test :refer :all])

(deftest addition-test
  (testing "Основное сложение"
    (is (= 4 (+ 2 2)))))
    
(run-tests)
```

Пример вывода после выполнения теста:

```
lein test user
Testing user

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

## Глубокое погружение
Подход Clojure к тестированию происходит из среды разработки с поддержкой REPL. Генеративное тестирование с использованием `test.check` и свойственное тестирование являются альтернативными стратегиями. Они автоматически генерируют тестовые случаи вместо ручного написания всех тестов. Реализация в значительной мере зависит от макросов, обеспечивая динамичную среду тестирования.

## Смотрите также
- [Тестирование в Clojure](https://clojure.org/guides/deps_and_cli#_testing)
- [Документация clojure.test на GitHub](https://github.com/clojure/clojure/blob/master/src/clj/clojure/test.clj)
- [Введение в свойственное тестирование с использованием `test.check`](https://github.com/clojure/test.check)
