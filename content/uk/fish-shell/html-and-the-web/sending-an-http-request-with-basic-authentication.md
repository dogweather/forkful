---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases:
- /uk/fish-shell/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:01:28.915228-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?
Відправка HTTP запитів з базовою автентифікацією - це процес передачі логіна та пароля в заголовок для доступу до ресурсів сервера. Програмісти роблять це для безпечного підключення до API, що вимагає авторизації.

## Як це зробити:
У Fish Shell використовуємо `curl` для створення запитів із базовою автентифікацією. Ось приклад:

```Fish Shell
set username "your_username"
set password "your_password"

curl -u $username:$password http://example.com
```

Це надсилає запит з логіном та паролем на `example.com`. Ось прикладний вивід:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
...
{
  "data": "Ваші дані тут"
}
```

## Поглиблений аналіз
Базова автентифікація, стандарт RFC 7617, була одним із перших методів верифікації. Існують альтернативи, як-от OAuth, що забезпечують більшу безпеку. У Fish Shell для автентифікації запитів використовують `curl`, бо це потужний інструмент, що підтримує багато протоколів передачі даних та методів авторизації. Під час відправки запиту з базовою автентифікацією, логін і пароль кодуються у Base64 і передаються у заголовку `Authorization`.

## Дивитись також
- [Curl Documentation](https://curl.se/docs/)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
