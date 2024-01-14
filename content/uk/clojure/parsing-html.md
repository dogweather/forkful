---
title:                "Clojure: Розбирання html"
simple_title:         "Розбирання html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

З парсінгом HTML ви можете отримати доступ до данних, вкладених у веб-сторінки. Це корисно для створення власних веб-скраперів або аналізу веб-даних.

## Як це зробити

```Clojure
(require '[clojure.data.xml :as xml])

(def html "<html><head><title>Мій перший веб-сайт</title></head><body><div><p>Вітаю! Це мій перший веб-сайт!</p></div></body></html>")

(xml/parse-str html)

;; {:tag :html, 
;;  :attrs {}, 
;;  :content [{:tag :head, 
;;             :attrs {}, 
;;             :content [{:tag :title, 
;;                        :attrs {}, 
;;                        :content ["Мій перший веб-сайт"]}]} 
;;            {:tag :body, 
;;             :attrs {}, 
;;             :content [{:tag :div, 
;;                        :attrs {}, 
;;                        :content [{:tag :p, 
;;                                   :attrs {}, 
;;                                   :content ["Вітаю! Це мій перший веб-сайт!"]}]}]}]}

(xml/select [:html :body :div :p] (xml/parse-str html)) 

;; [{:tag :p,
;;   :attrs {},
;;   :content ["Вітаю! Це мій перший веб-сайт!"]}]
```

## Глибоке занурення

Хоча є більш простіші способи парсінгу HTML, використання бібліотеки `clojure.data.xml` дозволяє більш гнучкий та сильний підхід до тривіального розбору даних. Ця бібліотека також повертає данні у вигляді структури даних Clojure, що дозволяє використовувати всю могутність мови для обробки та аналізу даних.

## Дивись також

- [Офіційна документація Clojure data.xml](https://clojure.github.io/data.xml/)
- [Вікі-сторінка Clojure для парсінгу HTML](https://github.com/functional-koans/clojure-koans/wiki)
- [Вступ до парсінгу HTML з Clojure](https://purelyfunctional.tv/article/parsing-html-in-clojure/)