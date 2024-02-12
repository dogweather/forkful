---
title:                "Загрузка веб-страницы"
aliases:
- /ru/clojure/downloading-a-web-page/
date:                  2024-01-28T23:57:21.134081-07:00
model:                 gpt-4-0125-preview
simple_title:         "Загрузка веб-страницы"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?
Скачивание веб-страницы подразумевает получение HTML с URL, так что ваша программа может работать с ним. Программисты делают это для извлечения данных, автоматизации веб-взаимодействий или проверки состояния сайта.

## Как сделать:
В Clojure вы можете использовать `clj-http` для быстрого скачивания веб-страницы. Вот простейший пример:

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; Используйте это так:
(defn -main []
  (println (download-page "http://example.com")))
```

Если попробуете, вы получите карту полную деталей. Главное содержится под `:body` и `:status`.

## Подробнее
Исторически, скачивание из веба было выполнением 'wget' или 'curl' в командной строке. Сейчас, языки программирования, как Clojure, абстрагируют это с помощью библиотек. `clj-http` - одна из таких библиотек, которая оборачивает Apache HttpComponents для Java в стиле функционального программирования Clojure.

Есть альтернативы? Конечно. Вы могли бы напрямую использовать `java.net.HttpURLConnection` или выбрать другую библиотеку, как `http-kit` – но `clj-http` удобен и содержит большинство необходимых вещей "из коробки".

Что касается основных моментов, `clj-http` превращает ваш запрос в HTTP сущность Java, делает вызов и возвращает ответ. За кулисами, он обрабатывает перенаправления, разбирает заголовки и управляет телом ответа, так что вы можете сосредоточиться на ваших данных, а не на механике.

## Смотрите также
- GitHub репозиторий clj-http: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Clojure http-kit для другого подхода: [http://www.http-kit.org](http://www.http-kit.org)
- Официальный сайт Clojure для дополнительной информации о языке: [https://clojure.org](https://clojure.org)
