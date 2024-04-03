---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:03.849912-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 HTML\
  \ \u0432 Clojure \u043C\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\
  \u043C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `clj-tagsoup`,\
  \ \u043E\u0431\u043E\u043B\u043E\u0447\u043A\u0443 \u0434\u043B\u044F \u0431\u0438\
  \u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 Tagsoup Java, \u043A\u043E\u0442\
  \u043E\u0440\u0430\u044F \u0443\u0434\u043E\u0431\u043D\u0430 \u0434\u043B\u044F\
  \ \u0440\u0430\u0437\u0431\u043E\u0440\u0430\u2026"
lastmod: '2024-03-13T22:44:44.345789-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0437\u0431\u043E\u0440\u0430 HTML \u0432\
  \ Clojure \u043C\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `clj-tagsoup`, \u043E\
  \u0431\u043E\u043B\u043E\u0447\u043A\u0443 \u0434\u043B\u044F \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0438 Tagsoup Java, \u043A\u043E\u0442\u043E\
  \u0440\u0430\u044F \u0443\u0434\u043E\u0431\u043D\u0430 \u0434\u043B\u044F \u0440\
  \u0430\u0437\u0431\u043E\u0440\u0430 \u0440\u0435\u0430\u043B\u044C\u043D\u043E\u0433\
  \u043E HTML."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Для разбора HTML в Clojure мы используем библиотеку `clj-tagsoup`, оболочку для библиотеки Tagsoup Java, которая удобна для разбора реального HTML.

Сначала добавьте зависимость clj-tagsoup в ваш проект:

```clojure
[clj-tagsoup "0.3.3"] ; Проверьте последнюю версию
```

Теперь давайте разберем некоторый HTML:

```clojure
(require '[clj-tagsoup.core :as tagsoup])

; Разбираем HTML и получаем вектор карт, представляющих разобранные элементы
(def parsed-html (tagsoup/parse-string "<html><body><p>Привет, мир!</p></body></html>"))

; Доступ к элементам
(println (first parsed-html))
```

Пример вывода:

```clojure
{:tag :html, :attrs {}, :content [...]}
```

Чтобы извлечь конкретные элементы, например параграфы:

```clojure
(defn extract-paragraphs [html]
  (let [parsed (tagsoup/parse-string html)]
    (filter #(= :p (:tag %)) parsed)))

; Использование
(extract-paragraphs "<p>Первый</p><p>Второй</p>")
```

## Подробнее
Разбор HTML в Clojure, как и на других языках, обычно включает навигацию по древовидной структуре. Раньше это могло быть запутанным. Библиотеки вроде Tagsoup упростили жизнь, обрабатывая своеобразный реальный HTML.

Функциональная природа Clojure позволяет нам гладко манипулировать данными HTML. Библиотеки вроде `clj-tagsoup` используют проверенные инструменты Java, добавляя элегантность Clojure.

Альтернативные библиотеки включают `Enlive` и `Hickory`. Enlive специализируется как на разборе, так и на шаблонизации, позволяя выполнять более сложные операции. Hickory преобразует HTML в структуры данных Clojure для тех, кто предпочитает чистое решение на Clojure.

Реализация сосредотачивается на простоте и декларативном стиле. Под капотом `clj-tagsoup` использует локаторы и навигаторы для обхода HTML, предоставляя более высокий уровень абстракции над прямой манипуляцией с DOM.

## См. также
- clj-tagsoup на GitHub: https://github.com/nathell/clj-tagsoup
- Tagsoup, базовая библиотека Java: https://github.com/McCLIM/cl-tagsoup
- Enlive, другая библиотека разбора HTML на Clojure: https://github.com/cgrand/enlive
- Hickory, проект Clojure для разбора HTML: https://github.com/davidsantiago/hickory
