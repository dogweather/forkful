---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:53.737287-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u043D\u0438\u0435\u043C Fetch API JavaScript."
lastmod: '2024-03-13T22:44:45.759944-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\u0440\
  \u0438\u043C\u0435\u0440 \u0441 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\
  \u0432\u0430\u043D\u0438\u0435\u043C Fetch API JavaScript."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430 \u0441 \u0431\u0430\u0437\u043E\u0432\u043E\u0439 \u0430\u0443\
  \u0442\u0435\u043D\u0442\u0438\u0444\u0438\u043A\u0430\u0446\u0438\u0435\u0439"
weight: 45
---

## Как это сделать:
Вот быстрый пример с использованием Fetch API JavaScript:

```javascript
const url = 'https://some-protected-resource.com/data';
const username = 'YourUsername';
const password = 'YourPassword';

const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { method: 'GET', headers: headers })
  .then(response => {
    if (response.ok) return response.json();
    throw new Error('Сетевой ответ был неудовлетворительным.');
  })
  .then(data => console.log(data))
  .catch(error => console.error('Ошибка Fetch: ', error));
```

Пример вывода (выведено в консоль):

```javascript
{
  "protected": "data",
  "moreData": 12345
}
```

## Подробнее
Прежде чем углубляться, давайте узнаем немного контекста. Базовая аутентификация - это одна из самых простых форм обеспечения безопасности веб-сервисов, при которой учетные данные отправляются в заголовках с каждым запросом.

Исторический контекст:
- Базовая HTTP-аутентификация - старый метод, впервые описанный в RFC 7617 в 2015 году, заменяющий еще более старый RFC 2617 из 1999 года.
- Она была широко использована из-за своей простоты, но не так безопасна без HTTPS, так как кодирование в base64 легко обратимо.

Альтернативы:
- OAuth: более безопасный и сложный стандарт для делегирования доступа, используемый в случаях, когда вам нужно предоставить доступ без разглашения парольных учетных данных.
- API ключи: одиночный токен, который легче управлять, чем сложные протоколы OAuth.
- Токены-носители: Особенно JWT (JSON Web Tokens), которые могут нести больше информации.

Детали реализации:
- Кодирование в Base64 преобразует строку имя пользователя:пароль в последовательность символов, которая более универсально передаваема.
- Всегда убедитесь, что соединение использует HTTPS, чтобы предотвратить перехват учетных данных.
- Современная разработка отдает предпочтение токенам и сессионным кукам для аутентификации, поскольку они более безопасны и универсальны.

## Смотрите также
- [Mozilla Developer Network - Авторизация](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [RFC 7617 - HTTP Basic Auth](https://tools.ietf.org/html/rfc7617)
- [Введение в OAuth 2.0](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
- [Введение в JSON Web Tokens (JWT)](https://jwt.io/introduction/)
