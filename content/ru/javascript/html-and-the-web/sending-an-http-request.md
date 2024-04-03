---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:39.188977-07:00
description: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 - \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \ \u043E\u0431\u0449\u0435\u043D\u0438\u044F \u0432\u0430\u0448\u0435\u0433\u043E\
  \ JavaScript \u043A\u043E\u0434\u0430 \u0441 \u0441\u0435\u0440\u0432\u0435\u0440\
  \u043E\u043C. \u042D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\u0442\u0441\u044F\
  \ \u0434\u043B\u044F \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\
  \u044B\u043C\u0438, \u043F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u044F \u0440\
  \u0435\u0441\u0443\u0440\u0441\u043E\u0432 \u0438\u043B\u0438 \u043E\u0442\u043F\
  \u0440\u0430\u0432\u043A\u0438 \u0434\u0430\u043D\u043D\u044B\u0445 \u043D\u0430\
  \u2026"
lastmod: '2024-03-13T22:44:45.754827-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\
  \u0440\u043E\u0441\u0430 - \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  \ \u043E\u0431\u0449\u0435\u043D\u0438\u044F \u0432\u0430\u0448\u0435\u0433\u043E\
  \ JavaScript \u043A\u043E\u0434\u0430 \u0441 \u0441\u0435\u0440\u0432\u0435\u0440\
  \u043E\u043C."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

## Что и Почему?

Отправка HTTP-запроса - это способ общения вашего JavaScript кода с сервером. Это делается для обмена данными, получения ресурсов или отправки данных на сервер для обработки.

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
