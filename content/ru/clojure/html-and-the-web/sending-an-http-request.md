---
title:                "Отправка HTTP-запроса"
aliases: - /ru/clojure/sending-an-http-request.md
date:                  2024-01-29T00:02:31.868766-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отправка HTTP-запроса - это способ, которым ваша программа просит другую систему предоставить данные или сервисы через интернет. Программисты делают это для взаимодействия с веб-API, получения ресурсов или коммуникации между сервисами.

## Как:
В Clojure вы можете отправить HTTP-запрос, используя клиент `clj-http`.

Сначала добавьте зависимость в ваш `project.clj`:
```clojure
[clj-http "3.12.3"]
```

Теперь отправим GET-запрос:
```clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://httpbin.org/get")]
  (println response))
```

Пример вывода:
```clojure
{:status 200, :headers {...}, :body "..."}
```

Чтобы отправить данные:
```clojure
(let [response (client/post "http://httpbin.org/post" {:form-params {:key "value"}})]
  (println response))
```

## Погружение
Отправка HTTP-запросов - это не новинка. Это так же старо, как и сам веб. Clojure, будучи современным Lisp, имеет несколько библиотек для отправки HTTP-запросов. `clj-http` - популярная из них, но существуют и другие, такие как `http-kit` или core Clojure `clj-http.client`.

`clj-http` использует под капотом Apache HttpComponents Client для Java. Она универсальна, но может казаться слишком "на Java". В качестве альтернативы, `http-kit` более легковесный и идиоматичный для Clojure, но менее функциональный.

Когда вы отправляете HTTP-запросы, вы делаете это через TCP/IP, который формирует ваши запросы в соответствии с устоявшимся протоколом. Этот универсальный стандарт позволяет вам взаимодействовать практически с любым веб-сервисом.

## Смотрите также
- репозиторий `clj-http` на GitHub: https://github.com/dakrone/clj-http
- Официальный сайт Clojure: https://clojure.org
- Документация HttpComponents Client: https://hc.apache.org/httpcomponents-client-ga/
- Для потребностей в реальном времени рассмотрите `http-kit`: http://www.http-kit.org
