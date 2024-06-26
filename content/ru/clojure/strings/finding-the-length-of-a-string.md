---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:34.444691-07:00
description: "\u041A\u0430\u043A: \u0427\u0442\u043E\u0431\u044B \u043F\u043E\u043B\
  \u0443\u0447\u0438\u0442\u044C \u0434\u043B\u0438\u043D\u0443 \u0441\u0442\u0440\
  \u043E\u043A\u0438 \u0432 Clojure, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0439\u0442\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `count`."
lastmod: '2024-03-13T22:44:44.333332-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u043F\u043E\u043B\u0443\u0447\u0438\u0442\
  \u044C \u0434\u043B\u0438\u043D\u0443 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432\
  \ Clojure, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 \u0444\
  \u0443\u043D\u043A\u0446\u0438\u044E `count`."
title: "\u041F\u043E\u0438\u0441\u043A \u0434\u043B\u0438\u043D\u044B \u0441\u0442\
  \u0440\u043E\u043A\u0438"
weight: 7
---

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
