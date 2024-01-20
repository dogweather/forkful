---
title:                "Надсилання http-запиту з базовою аутентифікацією"
html_title:           "Arduino: Надсилання http-запиту з базовою аутентифікацією"
simple_title:         "Надсилання http-запиту з базовою аутентифікацією"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Відправка HTTP-запиту з базовою аутентифікацією — процес, коли програма відправляє запит на сервер із логіном та паролем. Це робиться для перевірки прав користувача на доступ до певних ресурсів.

## Як це зробити:

В TypeScript це можна зробити за допомогою модуля axios. Ось приклад коду:

```TypeScript
import axios from 'axios';

let username = 'test';
let password = 'password';

let config = {
  auth: {
    username: username,
    password: password
  }
}

axios.get('https://www.example.com', config)
  .then((response) => {
    console.log(response);
  })
  .catch((error) => {
    console.error(error);
  });
```

В результаті на консоль виводиться відповідь від сервера.

## Занурення у деталі:

Історично, HTTP Basic Authentication був одним з перших методів аутентифікації, впроваджених в HTTP-протокол. Щоправда, він не надає сильного захисту і дуже часто використовується разом із SSL/TLS.

Існують і інші методи аутентифікації, такі як Digest Access Authentication, HTTPS та використання токенів.

Важливо пам'ятати, що ви ніколи не повинні відправляти паролі без зашифровування.

## Дивіться також:

1. [Axios Documentation](https://axios-http.com/docs/intro)
2. [HTTP Basic Authentication](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)
3. [HTTP Digest Access Authentication](https://en.wikipedia.org/wiki/Digest_access_authentication)