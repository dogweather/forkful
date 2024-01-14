---
title:                "Clojure: Надсилання запиту http з базовою аутентифікацією"
simple_title:         "Надсилання запиту http з базовою аутентифікацією"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запиту з базовою аутентифікацією - це важлива техніка для взаємодії з веб-службами, які потребують перевірки ідентифікації користувача. Використання цього виду аутентифікації дозволяє забезпечити захист інформації та зберегти приватність користувача при обміні даними.

## Як це зробити

Для надсилання HTTP-запиту з базовою аутентифікацією використовуються функції із бібліотеки `clj-http`:

```
(require '[clj-http.client :as client])

(defn- basic-auth-request
  "Виконує HTTP GET запит з базовою аутентифікацією"
  [url username password]
  (client/get url
              {:basic-auth [username password]}))

(basic-auth-request "https://example.com/api/resource" "username" "password")
```

В результаті отримуємо об'єкт запиту, який містить інформацію про статус відповіді та зміст.

## Глибоке занурення

Окрім базової аутентифікації, використовувана у прикладі, існують ще декілька видів аутентифікації, які можна використовувати при надсиланні HTTP-запитів. Наприклад, OAuth аутентифікація підходить для взаємодії з додатками, що вимагають доступу до облікових записів користувача. Також існує можливість передачі токена аутентифікації у хедері запиту замість ключа та паролю.

## Додаткові матеріали

* [Офіційна документація `clj-http` бібліотеки](https://github.com/dakrone/clj-http)
* [Документація з HTTP-аутентифікації](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)

## Дивіться також

* [Як виконати HTTP-запит з Clojure](https://miro.com/blog/how-to-make-http-requests-in-clojure/)
* [Використання базової аутентифікації в REST веб-службах](https://www.baeldung.com/rest-api-basic-authentication-with-spring-security)