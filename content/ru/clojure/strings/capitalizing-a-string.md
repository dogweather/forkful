---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:59.142168-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Clojure \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 \u0434\u043B\
  \u044F \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u043F\u0440\u0435\u043E\u0431\
  \u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u044F \u0441\u0442\u0440\u043E\u043A\
  \ \u0432 \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u0443\u044E \u0444\u043E\u0440\
  \u043C\u0443. \u0412\u044B \u0441\u043E\u0437\u0434\u0430\u0451\u0442\u0435 \u0441\
  \u0432\u043E\u044E \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\
  \u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438\u2026"
lastmod: '2024-03-13T22:44:44.318766-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 \u0434\u043B\u044F\
  \ \u043F\u0440\u044F\u043C\u043E\u0433\u043E \u043F\u0440\u0435\u043E\u0431\u0440\
  \u0430\u0437\u043E\u0432\u0430\u043D\u0438\u044F \u0441\u0442\u0440\u043E\u043A\
  \ \u0432 \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u0443\u044E \u0444\u043E\u0440\
  \u043C\u0443."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

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
