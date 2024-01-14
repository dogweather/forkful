---
title:                "Clojure: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

Зчому: Існує безліч причин, чому люди використовують HTTP запити в програмуванні. Наприклад, вони можуть бути використані для взаємодії зі зовнішніми API, отримання даних з веб-сторінок або навіть для збереження і передачі даних між різними додатками.

Як: У цьому прикладі ми будемо використовувати Clojure для створення HTTP запиту до API сервісу Github. Використовуючи бібліотеку "clj-http", ми зможемо легко і швидко зробити запит та обробити відповідь.

```Clojure
(ns my-app.core
  (:require [clj-http.client :as client]))

(defn make-request [url]
  (let [response (client/get url)]
    (println (:status response))
    (println (:body response))))

(def github-api-url "https://api.github.com")

(make-request github-api-url)
```

Вивід: 200
{
  "current_user_url": "https://api.github.com/user",
  "current_user_authorizations_html_url": "https://github.com/settings/connections/applications{/client_id}",
  "authorizations_url": "https://api.github.com/authorizations",
  "code_search_url": "https://api.github.com/search/code?q={query}{&page,per_page,sort,order}",
  "commit_search_url": "https://api.github.com/search/commits?q={query}{&page,per_page,sort,order}"
  ...


Глибокий занур: В цьому прикладі ми використовуємо метод "get" з бібліотеки "clj-http", щоб зробити GET запит до API сервісу Github. Метод "get" приймає URL та додаткові параметри, такі як заголовки, тіло запиту та інше.

```Clojure
(defn make-get-request [url]
  (let [response (client/get url {:basic-auth ["username" "password"]
                                  :headers {"Accept" "application/json"}})]
    (println (:status response))
    (println (:body response))))

(make-get-request github-api-url)
```

Вивід: 200
{
  "current_user_url": "https://api.github.com/user",
  "current_user_authorizations_html_url": "https://github.com/settings/connections/applications{/client_id}",
  "authorizations_url": "https://api.github.com/authorizations",
  "code_search_url": "https://api.github.com/search/code?q={query}{&page,per_page,sort,order}",
  "commit_search_url": "https://api.github.com/search/commits?q={query}{&page,per_page,sort,order}"
  ...


Дивіться також:
- [Офіційна документація бібліотеки "clj-http"](https://github.com/dakrone/clj-http)
- [Документація по використанню API сервісу Github](https://docs.github.com/en/rest)
- [Приклади використання HTTP запитів у Clojure](https://clojuredocs.org/clojure.core/http-kit)