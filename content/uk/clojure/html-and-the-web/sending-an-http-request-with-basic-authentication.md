---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases: - /uk/clojure/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:19.623118-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і Чому?
Відправлення HTTP-запитів із базовою аутентифікацією — це спосіб передачі логіна і паролю на сервер для віріфікації. Програмісти використовують це для доступу до захищених ресурсів.

## Як це зробити:
Clojure має бібліотеки, які спрощують HTTP запити. Ось як відправити такий запит із базовою аутентифікацією, використовуючи бібліотеку `clj-http`.

```Clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource [url username password]
  (let [auth-str (str username ":" password)
        encoded-auth-str (-> auth-str (.getBytes) java.util.Base64/getEncoder (.encodeToString))]
    (client/get url {:headers {"Authorization" (str "Basic " encoded-auth-str)}})))

;; Приклад використання:
(println (fetch-protected-resource "http://example.com/protected" "myUsername" "myPassword"))
```

Ця функція кодує логін і пароль в Base64 і вставляє їх у заголовок `Authorization`.

## Глибше Занурення:
Базова аутентифікація (`Basic Auth`) — старий, простий метод аутентифікації через HTTP. Ім'я користувача та пароль поєднують, кодують за допомогою Base64, і передають у заголовку `Authorization`. Його легко використати, але він не є найбезпечнішим методом, оскільки дані можна розкодувати. HTTPS мінімізує ризик.

Альтернативи базовій аутентифікації включають OAuth, API ключі, токени тощо. Ці методи забезпечують додаткові рівні безпеки і часто використовуються для сучасних API.

У Clojure та інших мовах програмування є багато бібліотек для відправки HTTP запитів. `clj-http` проста в користуванні і добре інтегрується з Clojure проектами. Ви напряму працюєте з заголовками, вмістом запитів, і параметрами — це дає можливість гнучко налаштовувати свої HTTP запити.

## Дивись також:
- [clj-http GitHub Repository](https://github.com/dakrone/clj-http) - бібліотека clj-http.
- [HTTP Basic Auth](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme) - MDN веб-документація по базовій аутентифікації.
- [ClojureDocs](https://clojuredocs.org/) - офіційна документація Clojure з прикладами від спільноти.
