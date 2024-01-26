---
title:                "Парсинг HTML"
date:                  2024-01-20T15:31:08.433097-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Парсинг HTML — це процес аналізу HTML коду для вилучення даних. Програмісти парсять HTML, щоб отримати доступ до інформації з веб-сторінок і використовувати її в своїх додатках або для аналізу.

## Як це робиться:
У Clojure для парсингу HTML можна використовувати бібліотеку `enlive`. Ось простий приклад її використання:

```Clojure
(require '[net.cgrand.enlive-html :as html])

(defn parse-html [html-content]
  (html/select (html/html-resource (java.io.ByteArrayInputStream. (.getBytes html-content))) [:div]))

(let [html-content "<html><body><div>Hello, World!</div></body></html>"]
  (println (parse-html html-content)))
```

Приклад виведення:
```
([{:tag :div, :attrs nil, :content ["Hello, World!"]}])
```

## Заглиблення:
Enlive — це потужний парсер HTML для Clojure, який був створений у 2009 році. Він дозволяє вам не тільки витягувати дані, але й шаблонізувати HTML. Серед альтернатив є `jsoup` та `hickory`, які також можна використовувати у проектах Clojure. Enlive працює на основі CSS селекторів, які дають гнучкість у виборі даних з HTML.

## Дивись також:
- [Jsoup - Java HTML Parser](https://jsoup.org/)
- [Hickory Github Page](https://github.com/davidsantiago/hickory)
- ClojureDocs - універсальний ресурс з прикладами та обговореннями для багатьох тем Clojure [ClojureDocs](https://clojuredocs.org/)
