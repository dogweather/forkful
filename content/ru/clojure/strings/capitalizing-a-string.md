---
title:                "Преобразование строки в верхний регистр"
aliases:
- ru/clojure/capitalizing-a-string.md
date:                  2024-01-28T23:55:59.142168-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в заглавную форму означает преобразование первой буквы строки в верхний регистр, а остальных - в нижний. Это делается для нормализации данных и улучшения читаемости, например, преобразование 'alice' в 'Alice' для имен.

## Как это сделать:
В Clojure нет встроенной функции для прямого преобразования строк в заглавную форму. Вы создаёте свою с помощью библиотеки `clojure.string`. Вот быстрый способ:

```clojure
(require '[clojure.string :as str])

(defn capitalize [s]
  (when s
    (str/capitalize s)))

(capitalize "hello world") ; => "Hello world"
```

Пример вывода для функции `capitalize`:

```clojure
(capitalize "clojure") ; => "Clojure"
(capitalize "123clojure") ; => "123clojure"
(capitalize "") ; => nil
(capitalize nil) ; => nil
```

## Погружение
Стандартная библиотека Clojure, `clojure.string`, предпочитает простоту. Таким образом, нет готовой функции `capitalize`, какую вы могли бы найти в других языках. Исторически Clojure опирается на методы строки Java, которые предлагают базовую манипуляцию, но не `capitalize`.

Этот недостаток толкает вас либо к написанию собственного решения, как показано выше, либо к использованию внешних библиотек. Также есть `capitalize` из `clojure.contrib.string`, исторически отдельная contrib библиотека, до того, как она была устаревшей и частично объединена с clojure.string в более поздних версиях.

Простота функции `str/capitalize` означает, что она занимается только первым символом. Для более нюансированного преобразования, например, преобразования в стиле названий или обработки международных символов, вам нужно написать собственное решение или воспользоваться Java библиотекой.

Вот альтернативная пользовательская функция, которая обрабатывает строки с несколькими словами:

```clojure
(defn title-case [s]
  (->> s
       (str/split #"\s+")
       (map str/capitalize)
       (str/join " ")))

(title-case "the lord of the rings") ; => "The Lord Of The Rings"
```

Снова, интернационализация (i18n) здесь не рассматривается; корректная обработка Unicode - это совсем другой вопрос, часто требующий специализированных библиотек.

## Смотрите также
- API строк Clojure: https://clojure.github.io/clojure/clojure.string-api.html
- Документация по строкам Java: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Архив библиотеки Clojure Contrib: https://github.com/clojure/clojure-contrib
- Исходный код `clojure.string`: https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj
