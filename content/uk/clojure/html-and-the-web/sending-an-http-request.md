---
date: 2024-01-20 17:59:35.442289-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) \u041F\u0440\u043E\u0441\u0442\u0438\u0439 GET \u0437\u0430\u043F\u0438\
  \u0442 \u043F\u043E\u0432\u0435\u0440\u0442\u0430\u0454 \u0434\u0430\u043D\u0456\
  , \u0430 POST \u0437\u0430\u043F\u0438\u0442 \u0432\u0456\u0434\u0441\u0438\u043B\
  \u0430\u0454 \u0434\u0430\u043D\u0456 \u043D\u0430 \u0441\u0435\u0440\u0432\u0435\
  \u0440. \u041A\u043E\u0434 \u0434\u0440\u0443\u043A\u0443\u0454 \u0432\u0456\u0434\
  \u043F\u043E\u0432\u0456\u0434\u0456 \u0434\u043B\u044F \u043A\u043E\u0436\u043D\
  \u043E\u0433\u043E \u0437\u0430\u043F\u0438\u0442\u0443."
lastmod: '2024-04-05T21:53:48.901503-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ \u041F\u0440\u043E\u0441\u0442\u0438\u0439 GET \u0437\u0430\u043F\u0438\u0442\
  \ \u043F\u043E\u0432\u0435\u0440\u0442\u0430\u0454 \u0434\u0430\u043D\u0456, \u0430\
  \ POST \u0437\u0430\u043F\u0438\u0442 \u0432\u0456\u0434\u0441\u0438\u043B\u0430\
  \u0454 \u0434\u0430\u043D\u0456 \u043D\u0430 \u0441\u0435\u0440\u0432\u0435\u0440\
  ."
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443"
weight: 44
---

## How to: (Як це зробити:)
```Clojure
(require '[clj-http.client :as client])

;; Створення простого GET запиту
(def response (client/get "https://api.example.com/data"))
(println response)

;; Відправлення POST запиту з параметрами
(def post-response (client/post "https://api.example.com/submit"
                  {:form-params {:key "value"}}))
(println post-response)
```
Простий GET запит повертає дані, а POST запит відсилає дані на сервер. Код друкує відповіді для кожного запиту.

## Deep Dive (Поглиблений Розгляд):
HTTP запити існують з ранніх днів Інтернету. Вони частина протоколу TCP/IP, що був стандартом комунікацій з 1980-х. В Clojure, бібліотека `clj-http` часто використовується для роботи з HTTP запитами завдяки своїй зручності та гнучкості.

Альтернативою є `http-kit` або низькорівневі Java бібліотеки, як-от `Apache HttpClient`.

HTTP запити в Clojure, як і в інших JVM мовах, кінцево виконуються через Java-класи і механізми, але Clojure обгортки значно спрощують процес.

## See Also (Додатково):
- [clj-http GitHub repository](https://github.com/dakrone/clj-http)
- [The Java™ Tutorials - All About Sockets](https://docs.oracle.com/javase/tutorial/networking/sockets/index.html)
- [HTTP-kit project](http://www.http-kit.org/)
