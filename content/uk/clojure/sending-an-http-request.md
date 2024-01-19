---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що це та навіщо потрібно?
Надсилання HTTP-запиту – це процес, під час якого ваш код взаємодіє з веб-сторінками чи API. Завдяки цьому можливо отримати, змінити та надсилати дані, збережені в інтернеті.

## Як це зробити:
Ось простий приклад, як надіслати HTTP GET-запит у Clojure:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com" {:as :json})]
  (println (:status response))
  (println (:body response)))
```
У вищезазначеному коді на початку ми імпортуємо HTTP-клієнта. І надсилаємо HTTP-запит до `http://example.com`. Параматер `{ :as :json }` говорить, що ми очікуємо відповідь у форматі JSON. Наша відповідь буде об'єктом, властивостями якого будуть статус та тіло відповіді.

## Занурюємося глибше:
Надсилання HTTP запитів мало велике значення для формування сучасного Інтернету. Це насправді основа всіх веб-з'єднань. 

Що стосується альтернатив, Clojure пропонує багато інших бібліотеки, таких як `http-kit` і `aleph`. Вибір залежить від ваших специфічних потреб.

А ще, HTTP-клієнт в Clojure використовує Java HTTP-клієнта під капотом. Завдяки цьому ми можемо використовувати всю потужність JVM при роботі з HTTP.

## Дивіться також:
1. [Офіційна документація clj-http](https://github.com/dakrone/clj-http)
2. [Документація по http-kit](http://www.http-kit.org/)
3. [Документація по aleph](https://aleph.io/)

Нагадую, що найкращий спосіб вивчити надсилання HTTP-запитів – це погратися з різними видами запитів самостійно. Так що - витягуйте свої редактори коду та веселіться, кодячи!