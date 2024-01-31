---
title:                "Преобразование строки в нижний регистр"
date:                  2024-01-28T23:56:25.301140-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в нижний регистр означает преобразование всех символов в тексте в их варианты в нижнем регистре, например, преобразование "Hello, World!" в "hello, world!". Программисты делают это для обеспечения согласованности, особенно в задачах, таких как сравнение пользовательского ввода, где регистр не должен иметь значения.

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
