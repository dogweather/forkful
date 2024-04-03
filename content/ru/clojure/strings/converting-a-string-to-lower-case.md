---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:25.301140-07:00
description: "\u041A\u0430\u043A: \u0412 Clojure, \u0447\u0442\u043E\u0431\u044B \u043F\
  \u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u0442\u044C \u0441\u0442\
  \u0440\u043E\u043A\u0443 \u0432 \u043D\u0438\u0436\u043D\u0438\u0439 \u0440\u0435\
  \u0433\u0438\u0441\u0442\u0440, \u0432\u044B \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u0435\u0442\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `clojure.string/lower-case`.\
  \ \u041F\u043E\u0441\u043C\u043E\u0442\u0440\u0438\u0442\u0435, \u043D\u0430\u0441\
  \u043A\u043E\u043B\u044C\u043A\u043E \u044D\u0442\u043E \u043F\u0440\u043E\u0441\
  \u0442\u043E."
lastmod: '2024-03-13T22:44:44.326276-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure, \u0447\u0442\u043E\u0431\u044B \u043F\u0440\u0435\u043E\u0431\
  \u0440\u0430\u0437\u043E\u0432\u0430\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0443\
  \ \u0432 \u043D\u0438\u0436\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\
  \u0440, \u0432\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `clojure.string/lower-case`."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как:
В Clojure, чтобы преобразовать строку в нижний регистр, вы используете функцию `clojure.string/lower-case`. Посмотрите, насколько это просто:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!") ; => "hello, world!"
```

Вывод простой:

```clojure
"hello, world!"
```

## Подробнее
Исторически, преобразование регистра существовало с ранних времен вычислительной техники для гармонизации обработки текстовых данных. В Clojure функция `clojure.string/lower-case` является частью библиотеки `clojure.string`, коллекции утилит для манипуляции со строками, включенной в основной язык.

Альтернативы `clojure.string/lower-case` включают создание собственной функции через маппинг с манипуляцией `char`, но это изобретение колеса, когда у вас есть встроенная функция, которая оптимизирована и хорошо протестирована.

Внутри `clojure.string/lower-case` передает основную работу методу `toLowerCase` Java, поскольку Clojure работает на Java Virtual Machine (JVM). Это обеспечивает высокую производительность, так как использует зрелые библиотеки Java.

## См. Также
- API `clojure.string` в Clojure: https://clojuredocs.org/clojure.string
- Метод `String.toLowerCase()` в Java: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
