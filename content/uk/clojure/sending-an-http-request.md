---
title:                "Надсилання запиту http."
html_title:           "Clojure: Надсилання запиту http."
simple_title:         "Надсилання запиту http."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
Запити HTTP є необхідною частиною веб-розробки. Вони дозволяють отримувати інформацію з інших сайтів або сервісів, що робить їх корисними для побудови багатофункціональних додатків.

## Як
Найпростішим способом відправити запит HTTP в Clojure є використання функції `clj-http.client/request`. Приклад коду:

```Clojure
(ns my-app.core
  (:require [clj-http.client :as http]))

(def response (http/request {:method :get :url "https://www.example.com"}))

(println (:body response))
```

В даному прикладі ми використовуємо функцію `request` для відправлення GET-запиту на сайт https://www.example.com. Ми отримуємо об'єкт відповіді, з якого ми можемо отримати тіло відповіді за допомогою ключа `:body`.

## Глибше
Функція `request` надає багато параметрів, які можна використовувати для налаштування запиту. Деякі з них:

- `:method` - метод запиту (GET, POST, PUT, DELETE тощо)
- `:url` - URL-адреса для відправки запиту
- `:body` - тіло запиту, яке може бути різного типу (рядок, мапа тощо)
- `:headers` - заголовки запиту, представлені у вигляді мапи
- `:query-params` - параметри запиту для GET-запитів

Крім того, за допомогою параметра `:as` ми можемо задати формат відповіді, в якому ми хочемо, щоб були повернуті дані. Наприклад, `:as :json` поверне JSON-об'єкт, а `:as :string` поверне рядок.

## Дивіться також
- [Офіційна документація по функції request](https://github.com/dakrone/clj-http/blob/master/doc/http/client.md#cljhtt