---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:39.188977-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: JavaScript \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ API `fetch` \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0438\
  \ \u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432. \u0412\u043E\u0442 \u043A\u0430\
  \u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C \u043F\u0440\u043E\u0441\u0442\
  \u043E\u0439 GET-\u0437\u0430\u043F\u0440\u043E\u0441."
lastmod: '2024-03-13T22:44:45.754827-06:00'
model: gpt-4-0125-preview
summary: "JavaScript \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ API `fetch` \u0434\u043B\u044F \u043E\u0442\u043F\u0440\u0430\u0432\u043A\u0438\
  \ \u0437\u0430\u043F\u0440\u043E\u0441\u043E\u0432."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Как это сделать:
JavaScript использует API `fetch` для отправки запросов. Вот как сделать простой GET-запрос:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts/1')
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Ошибка:', err));
```

На выходе будет JSON-данные с URL. Просто, правда?

А для POST-запроса:

```javascript
fetch('https://jsonplaceholder.typicode.com/posts', {
  method: 'POST',
  body: JSON.stringify({
    title: 'foo',
    body: 'bar',
    userId: 1,
  }),
  headers: {
    'Content-type': 'application/json; charset=UTF-8',
  },
})
  .then(response => response.json())
  .then(json => console.log(json))
  .catch(err => console.error('Ошибка:', err));
```

Это отправляет новые данные и выводит ответ сервера.

## Погружение
HTTP-запросы существуют с начала веба — подумайте об HTML-формах. XMLHttpRequest (XHR) когда-то был основным методом отправки запросов в JavaScript, но он неудобен.

На смену пришел `fetch`, современный подход на основе промисов, который делает его более чистым и надежным. В отличие от XHR, `fetch` обрабатывает как запросы, так и ответы в едином API и встроен в язык, без необходимости использования библиотек.

Альтернативы? Конечно. Библиотеки вроде Axios или Ajax jQuery все еще используются. Они предлагают некоторые удобства в синтаксисе и решения для конкретных проблем, но `fetch` это родной и, в целом, путь вперед.

Детали реализации? Не забывайте обрабатывать ошибки, работать с различными типами ответов и быть в курсе правил обмена ресурсами между разными источниками (CORS).

## Смотрите также
- Документация по API `fetch` на MDN: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Использование промисов в JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Using_promises
- Узнать о CORS: https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
