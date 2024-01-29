---
title:                "Удаление символов, соответствующих шаблону"
date:                  2024-01-28T23:57:13.456978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Удаление символов, соответствующих шаблону, означает очистку определенных последовательностей в строке. Программисты делают это для очистки данных, обеспечения соответствия форматам или удаления нежелательной информации.

## Как это делать:

Чтобы удалить символы, используя шаблон в Clojure, вы применяете регулярные выражения с функциями `re-seq`, `re-find` или `re-matches` в сочетании с `clojure.string/replace`.

```Clojure
(require '[clojure.string :as str])

;; Удаление всех цифр из строки
(str/replace "He110 W0rld" #"\d+" "")
;; => "He Wrd"

;; Удаление определенных специальных символов
(str/replace "Hello, World! #Clojure" #"[,!#]" "")
;; => "Hello World Clojure"

;; Оставить только словесные символы и пробелы
(str/replace "Email@Example.com" #"[^\w\s]+" "")
;; => "EmailExamplecom"
```

## Подробнее
Clojure, отражая своё наследие Lisp, отличается в символической обработке, делая работу с шаблонами простой. Представленный в 2007 году, он опирается на возможности Java Virtual Machine (JVM), используя мощный класс `Pattern` от Java для регулярных выражений.

Альтернативы регулярным выражениям включают в себя ручную итерацию и манипуляцию со строками, но они часто бывают более многословными и подвержены ошибкам. Библиотеки вроде `clojure.spec` могут помочь валидировать и приводить данные в соответствие с шаблонами без непосредственного удаления.

Операции удаления обычно очень эффективны, но нужно быть внимательным к сложности регулярных выражений, которые могут превратить задачу O(n) в гораздо худшую. Неизменяемость строк в Clojure означает, что каждый `replace` создаёт новую строку, что стоит учитывать для приложений, чувствительных к памяти.

## Смотрите также
- [API строк Clojure](https://clojure.github.io/clojure/clojure.string-api.html)
- [Класс Java Pattern](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [clojure.spec](https://clojure.org/guides/spec)
