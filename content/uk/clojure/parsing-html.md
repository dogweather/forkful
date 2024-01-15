---
title:                "Розбір html"
html_title:           "Clojure: Розбір html"
simple_title:         "Розбір html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-html.md"
---

{{< edit_this_page >}}

# Чому

Електронні сторінки у Інтернеті працюють на основі мови розмітки гіпертексту (HTML). Парсинг HTML дозволяє нам зчитувати, розуміти та обробляти ці сторінки, що дає нам безмежні можливості для автоматизації та аналізу веб-контенту.

# Як це зробити

```Clojure
(ns parsing-html.core
  (:require [net.cgrand.enlive-html :as enlive]
            [clojure.java.io :as io]))

;; зчитуємо та парсимо веб-сторінку
(def page (enlive/html-resource (io/file "index.html")))

;; знаходимо всі посилання на сторінці та виводимо їх
(def links (enlive/select page [:a :href]))
(enlive/emit* links) ;; ["www.example.com", "www.clojure.org"]

;; отримуємо вміст певної секції сторінки та виводимо його
(def content (enlive/select-one page [:div#main-content]))
(enlive/text content) ;; "Це головний вміст сторінки"
```

# Глибше погляду

При парсингу HTML, ми можемо використовувати CSS селектори для точного визначення елементів, які нас цікавлять. Крім того, бібліотека Enlive надає можливість використання шаблонів для зручної обробки веб-сторінок.

# Дивись також

- [Офіційна документація Clojure](https://clojure.org)
- [Basho Technologies - стаття про парсинг HTML у Clojure](https://docs.basho.com/riak/kv/2.2.3/developing/api/languages/clojure/)
- [Enlive - документація та приклади](https://github.com/cgrand/enlive)