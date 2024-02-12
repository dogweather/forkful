---
title:                "Аналіз HTML"
aliases:
- /uk/clojure/parsing-html.md
date:                  2024-02-03T19:11:55.572576-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Парсинг HTML у Clojure передбачає програмний вилучення інформації з HTML-документів. Програмісти роблять це для доступу, маніпуляції або моніторингу веб-контенту динамічно, автоматизуючи задачі чи передаючи дані в додатки.

## Як зробити:

Clojure не має вбудованих засобів для парсингу HTML, але ви можете використовувати бібліотеки Java або обгортки Clojure, такі як `enlive` або `hickory`. Ось як використати обидва:

### Використання Enlive:

Enlive є популярним рішенням для парсингу HTML та веб-скрапінгу. Спочатку включіть його в залежності вашого проекту:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Потім ви можете парсити і навігувати по HTML так:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Цей фрагмент отримує HTML-сторінку і вибирає всі елементи `<div>` з класом `some-class`.

Вивід може виглядати так:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Ось деякий контент."]})
```

### Використання Hickory:

Hickory надає спосіб парсити HTML у формат, який легше обробляти в Clojure. Додайте Hickory до залежностей вашого проекту:

```clojure
[hickory "0.7.1"]
```

Ось простий приклад:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Парсимо HTML у формат Hickory
(let [doc (hickory/parse "<html><body><div id='main'>Привіт, світе!</div></body></html>")]
  ;; Вибираємо div з id 'main'
  (select/select (select/id "main") doc))
```

Цей код парсить простий рядок HTML і використовує CSS-селектор для пошуку `div` з ID `main`.

Приклад виводу:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Привіт, світе!"]}]
```

Як `enlive`, так і `hickory` пропонують міцні рішення для парсингу HTML у Clojure, причому `enlive` фокусується більше на темплейтингу, а `hickory` - на трансформації даних.
