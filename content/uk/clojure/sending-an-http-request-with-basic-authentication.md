---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що та чому?

Надсилання HTTP-запиту із базовою аутентифікацією — це процес автентифікації користувача за допомогою його логіну та паролю у вигляді HTTP-запиту. Це запобігає несанкціонованому доступу до ресурсів сервера.

## Як це зробити:

Clojure пропонує потужну бібліотеку — clj-http — для роботи з HTTP. Щоб надіслати запит із базовою аутентифікацією, скористайтеся наступним кодом:

```Clojure
(require '[clj-http.client :as client])

(let [{:keys [status body]} (client/get "http://your-website.com" {:basic-auth ["username" "password"]})]
  (println status)
  (println body))
```

В результаті виведеться статус та тіло відповіді сервера.

## Поглиблений аналіз:

1. Історія: Стандарт HTTP Basic Authentication був вперше описаний в RFC 1945 у 1996 році, але з тих пір його значно модернізували.

2. Альтернативи: OAuth — це сучасний стандарт, який надає більше гнучкості і безпеки порівняно з базовою аутентифікацією.

3. Деталі виконання: Clojure використовує Java-бібліотеку Apache HttpComponents для низькорівневої реалізації HTTP-запитів.

## Додатково:

1. [Документація проікту clj-http.](https://github.com/dakrone/clj-http)
2. [Огляд різних типів аутентифікації в HTTP.](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
3. [RFC 1945: Hypertext Transfer Protocol -- HTTP/1.0](https://datatracker.ietf.org/doc/html/rfc1945)