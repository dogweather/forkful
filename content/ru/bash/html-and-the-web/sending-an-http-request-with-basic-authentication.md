---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:40.817097-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\u043E\u0433\u0440\
  \u0443\u0437\u0438\u043C\u0441\u044F \u0432 \u043A\u043E\u0434. \u041C\u044B \u0431\
  \u0443\u0434\u0435\u043C \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u0442\u044C `curl`, \u043E\u0431\u0449\u0435\u0438\u0437\u0432\u0435\u0441\u0442\
  \u043D\u044B\u0439 \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\
  \ \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\u0442\u0440\u043E\
  \u043A\u0438. \u0417\u0430\u043C\u0435\u043D\u0438\u0442\u0435 `username:password`\
  \ \u043D\u0430 \u0432\u0430\u0448\u0438\u2026"
lastmod: '2024-03-13T22:44:45.369713-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\u043E\u0433\u0440\u0443\
  \u0437\u0438\u043C\u0441\u044F \u0432 \u043A\u043E\u0434."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Давайте погрузимся в код. Мы будем использовать `curl`, общеизвестный инструмент командной строки. Замените `username:password` на ваши учетные данные и `http://example.com/resource` на ваш целевой URL.

```Bash
curl -u username:password http://example.com/resource
```

Или заранее закодируйте свои учетные данные в base64 и используйте их так:

```Bash
# Кодирование учетных данных
credentials=$(echo -n username:password | base64)

# Отправка запроса
curl -H "Authorization: Basic $credentials" http://example.com/resource
```

Пример вывода для успешного запроса может выглядеть вот так:

```Bash
{
  "data": "Некоторая ограниченная информация",
  "message": "Доступ предоставлен"
}
```

## Подробнее
Исторически базовая аутентификация была частью HTTP с ранних дней, но она не без недостатков – в основном из-за уязвимости, если не использовать ее по защищенному каналу, такому как HTTPS.

Альтернативами являются OAuth, который более безопасен и предоставляет более детальный контроль над тем, к чему осуществляется доступ. Дайджест-аутентификация - еще один вариант, при котором отправляются хэшированные учетные данные, а не текст.

Что касается механики, когда вы отправляете учетные данные базовой аутентификации, они включаются в HTTP-заголовок, закодированный в Base64. Это не шифрование, поэтому если вы не используете HTTPS, любой, кто перехватит запрос, может легко его декодировать. Использование HTTPS обеспечивает безопасную передачу, шифруя все между клиентом и сервером.

## Смотрите также
- Официальная документация cURL: https://curl.haxx.se/docs/manpage.html
- HTTP Authentication: Basic and Digest Access Authentication (IETF RFC 7617): https://tools.ietf.org/html/rfc7617
- Введение в OAuth: https://oauth.net/2/introduction/
