---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:55.572576-07:00
description: "\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Clojure \u043D\
  \u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\
  \u0445 \u0437\u0430\u0441\u043E\u0431\u0456\u0432 \u0434\u043B\u044F \u043F\u0430\
  \u0440\u0441\u0438\u043D\u0433\u0443 HTML, \u0430\u043B\u0435 \u0432\u0438 \u043C\
  \u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0432\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0438 Java \u0430\u0431\u043E \u043E\u0431\u0433\u043E\u0440\u0442\u043A\u0438\
  \ Clojure, \u0442\u0430\u043A\u0456 \u044F\u043A `enlive` \u0430\u0431\u043E\u2026"
lastmod: '2024-03-13T22:44:48.649476-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u0438\u0445 \u0437\u0430\u0441\u043E\u0431\u0456\u0432 \u0434\u043B\
  \u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 HTML, \u0430\u043B\u0435\
  \ \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A\u0438 Java \u0430\u0431\u043E \u043E\u0431\u0433\u043E\
  \u0440\u0442\u043A\u0438 Clojure, \u0442\u0430\u043A\u0456 \u044F\u043A `enlive`\
  \ \u0430\u0431\u043E `hickory`."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

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
