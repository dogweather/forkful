---
title:                "Разбор HTML"
aliases:
- /ru/clojure/parsing-html/
date:                  2024-01-29T00:00:03.849912-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Разбор HTML - это процесс преобразования строки HTML в структуру данных, которую ваша программа может понимать и манипулировать. Программисты делают это для взаимодействия с веб-контентом, его извлечения и модификации.

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
