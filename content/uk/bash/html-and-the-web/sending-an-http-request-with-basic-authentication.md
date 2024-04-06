---
date: 2024-01-20 18:01:14.758983-07:00
description: "\u042F\u043A \u0441\u0430\u043C\u0435: Sample output."
lastmod: '2024-04-05T21:53:49.717532-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041D\u0430\u0434\u0441\u0438\u043B\u0430\u043D\u043D\u044F HTTP-\u0437\u0430\
  \u043F\u0438\u0442\u0443 \u0437 \u0431\u0430\u0437\u043E\u0432\u043E\u044E \u0430\
  \u0432\u0442\u0435\u043D\u0442\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0454\u044E"
weight: 45
---

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
