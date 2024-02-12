---
title:                "Поиск длины строки"
aliases:
- /ru/clojure/finding-the-length-of-a-string.md
date:                  2024-01-28T23:57:34.444691-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск длины строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Нахождение длины строки в Clojure возвращает число символов в этой строке. Программистам часто требуется эта информация для проверки входных данных, перебора символов или для задач манипуляции со строками.

## Как:
Чтобы получить длину строки в Clojure, используйте функцию `count`:

```clojure
(count "Hello, World!") ;=> 13
```

Это означает, что в "Hello, World!" 13 символов.

## Подробнее
Функция `count` является предпочтительной в Clojure для нахождения количества элементов в коллекции, а строки не являются исключением, поскольку их можно рассматривать как последовательность символов. Исторически `count` была частью Clojure с её ранних версий, что отражает её корни в Lisp, где операции с длиной списков являются обычным делом.

Альтернативой `count` может быть использование интероперабельности с Java, поскольку Clojure работает на JVM:

```clojure
(.length "Hello, World!") ;=> 13
```

Это вызывает метод `.length` из класса String в Java. Хотя эта альтернатива существует, использование `count` является более идиоматичным для Clojure.

Стоит отметить, что `count` является операцией O(1) для строк, что означает, что она занимает постоянное количество времени независимо от длины строки, поскольку метаданные о длине строки кэшируются.

## Смотрите также
- Официальная документация Clojure о `count`: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/count)
