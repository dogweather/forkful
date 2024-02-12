---
title:                "Написание тестов"
aliases:
- /ru/clojure/writing-tests/
date:                  2024-01-29T00:05:43.408399-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Написание тестов означает создание кода, который проверяет, работает ли другой код так, как ожидается. Программисты делают это, чтобы выявлять ошибки, обеспечивать надежность и избегать головной боли в будущем.

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
