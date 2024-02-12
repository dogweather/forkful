---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
aliases: - /uk/bash/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:14.758983-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Відправлення HTTP-запиту з базовою аутентифікацією - це спосіб передачі логіна та паролю на сервер для доступу до захищених ресурсів. Програмісти використовують це, щоб забезпечити безпечний обмін даними.

## Як саме:
```Bash
# Встановлення змінних для користувача та пароля
USER='myusername'
PASSWORD='mypassword'

# Кодування користувача і пароля у форматі Base64
ENCODED_CREDENTIALS=$(echo -n "$USER:$PASSWORD" | base64)

# Відправлення запиту GET з використанням Basic Auth
RESPONSE=$(curl -H "Authorization: Basic $ENCODED_CREDENTIALS" -X GET http://example.com/resource)

# Виведення відповіді
echo $RESPONSE
```
Sample output:
```Bash
{"status":"success","message":"Authenticated"}
```

## Глибше занурення
Колись HTTP Basic Auth був широко поширеним стандартом для захисту веб-ресурсів. Простота його реалізації та підтримка на багатьох платформах забезпечила популярність. Сучасні альтернативи, такі як OAuth та JWT, пропонують більшу безпеку та гнучкість. При використанні Basic Auth важливо забезпечити шифрування з'єднання з SSL/TLS, щоб уникнути перехоплення облікових даних.

## Дивіться також
- [cURL Documentation](https://curl.haxx.se/docs/manpage.html)
- [HTTP authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [Base64 Encoding](https://www.base64encode.org/)
- [Understanding Basic Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
