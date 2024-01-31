---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:20.644683-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Регулярные выражения (regex) используются для поиска, сопоставления и манипуляции строками. Они применяются из-за их гибкости и эффективности в задачах обработки текста.

## Как использовать:

```clojure
(require '[clojure.string :as str])

;; 1. Сопоставление
(re-matches #"\d+" "123")               ;; => "123"
(re-matches #"\d+" "abc")               ;; => nil

;; 2. Поиск
(re-find #"\d+" "Заказ 100 яблок")     ;; => "100"

;; 3. Замена
(str/replace "2023-03-15" #"\d{4}" "ГГГГ") ;; => "ГГГГ-03-15"

;; 4. Разделение
(str/split "один,два,три" #",")       ;; => ["один" "два" "три"]
```

## Погружение в детали
Регулярные выражения имеют богатую историю, восходящую к теоретическим работам 1950-х годов Стивена Коула Клини. Альтернативы regex включают функции строки, такие как `indexOf`, `substring`, и библиотеки разбора; однако, regex часто предоставляет более краткое решение. Возможности регулярных выражений в Clojure основаны на классе `Pattern` Java, предоставляя мощный поиск по шаблону непосредственно в языке.

## См. также
- [ClojureDocs о регулярных выражениях](https://clojuredocs.org/clojure.core/re-find)
- [Класс Java Pattern](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
