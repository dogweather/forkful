---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке їй Навіщо?

Відправка HTTP-запиту з基овою аутентифікацією - це процес, при якому програма відправляє запит на веб-сервер із передачею імени користувача та пароля для аутентифікації. Програмісти роблять це для безпечного доступу до захищених ресурсів сервера.

## Як саме:

Отже, ось як ви можете відправити HTTP-запит з базовою аутентифікацією у Fish Shell:

```Fish Shell
set username твоє_ім'я_користувача
set password твій_пароль
set url http://yoururl.com

curl -u $username:$password $url
```
Цей код створить HTTP-запит до вказаного URL з вашим ім'ям користувача і паролем.

## Вглиблене розуміння: 

Відправка HTTP-запитів з базовою аутентифікацією була розроблена в 1990-ті роки як частина оригінального стандарту HTTP. Альтернативи включають Digest Authentication і OAuth, які обоє використовують розширені механізми безпеки.

У тимчасовому надсиланні HTTP-запитів з базовою аутентифікацією, ім'я користувача та пароль об'єднуються з двокрапкою, кодуються у форматі Base64, а потім відправляються на сервер в заголовку `Authorization`.

## Посилання на інші джерела:

1. [HTTP Authentication (MDN)](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
2. [Basic Access Authentication (Wikipedia)](https://en.wikipedia.org/wiki/Basic_access_authentication)
3. [Fish Shell Docs](https://fishshell.com/docs/current/index.html)