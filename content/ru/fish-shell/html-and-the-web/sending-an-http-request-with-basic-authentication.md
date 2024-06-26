---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:49.446863-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Fish Shell \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0439\u0442\u0435 `curl` \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\
  \u043A\u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\
  \u0437\u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\
  \u043A\u0430\u0446\u0438\u0435\u0439. \u0417\u0430\u043C\u0435\u043D\u0438\u0442\
  \u0435 `username`, `password` \u0438 `the_url`."
lastmod: '2024-03-13T22:44:45.839541-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Fish Shell \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\
  \u0442\u0435 `curl` \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\
  \u0438 HTTP-\u0437\u0430\u043F\u0440\u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\
  \u043E\u0432\u043E\u0439 \u0430\u0443\u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\
  \u0430\u0446\u0438\u0435\u0439."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
В Fish Shell используйте `curl` для отправки HTTP-запроса с базовой аутентификацией. Замените `username`, `password` и `the_url`:

```Fish Shell
set -x AUTH (echo -n "username:password" | base64)
curl -H "Authorization: Basic $AUTH" the_url
```

Или позвольте `curl` выполнить кодировку:

```Fish Shell
curl -u username:password the_url
```

Пример вывода может выглядеть так:

```Fish Shell
HTTP/1.1 200 OK
Content-Type: application/json
{
  "message": "Аутентификация прошла успешно."
}
```

## Подробнее
Базовая аутентификация является частью протокола HTTP, существующего с начала 90-х годов. Несмотря на легкость реализации, она менее безопасна из-за того, что учетные данные кодируются только в base64, а не шифруются. HTTPS помогает, но это не панацея.

К альтернативам относится OAuth, который использует токены вместо учетных данных, добавляя слои безопасности. Для большей безопасности рассмотрите возможность использования API-ключей или JWT (JSON Web Tokens).

С Fish Shell мы работаем с `curl`, мощным инструментом, поддерживающим различные протоколы и методы аутентификации. Флаг `-u` удобен, но избегайте жесткого кодирования учетных данных; вместо этого используйте переменные среды или файлы конфигурации с соответствующими разрешениями.

## Смотрите также:
- Документация cURL: https://curl.se/docs/httpscripting.html
- RFC базовой аутентификации HTTP: https://tools.ietf.org/html/rfc7617
- Документация Fish Shell: https://fishshell.com/docs/current/index.html
- Понимание JWT: https://jwt.io/introduction/
