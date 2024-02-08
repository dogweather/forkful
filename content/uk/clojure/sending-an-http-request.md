---
title:                "Надсилання HTTP-запиту"
aliases:
- uk/clojure/sending-an-http-request.md
date:                  2024-01-20T17:59:35.442289-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
HTTP запит – це спосіб звернення до сервера за даними чи послугами. Програмісти відсилають HTTP запити, щоб спілкуватися з веб-серверами, отримувати інформацію, відправляти дані та інтегрувати системи.

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
