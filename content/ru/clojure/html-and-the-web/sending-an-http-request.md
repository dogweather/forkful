---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:31.868766-07:00
description: "\u041A\u0430\u043A: \u0412 Clojure \u0432\u044B \u043C\u043E\u0436\u0435\
  \u0442\u0435 \u043E\u0442\u043F\u0440\u0430\u0432\u0438\u0442\u044C HTTP-\u0437\u0430\
  \u043F\u0440\u043E\u0441, \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F\
  \ \u043A\u043B\u0438\u0435\u043D\u0442 `clj-http`. \u0421\u043D\u0430\u0447\u0430\
  \u043B\u0430 \u0434\u043E\u0431\u0430\u0432\u044C\u0442\u0435 \u0437\u0430\u0432\
  \u0438\u0441\u0438\u043C\u043E\u0441\u0442\u044C \u0432 \u0432\u0430\u0448 `project.clj`."
lastmod: '2024-03-13T22:44:44.343922-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u043E\u0442\
  \u043F\u0440\u0430\u0432\u0438\u0442\u044C HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\
  , \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044F \u043A\u043B\u0438\u0435\
  \u043D\u0442 `clj-http`."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

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
