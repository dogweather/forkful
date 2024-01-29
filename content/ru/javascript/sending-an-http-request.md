---
title:                "Отправка HTTP-запроса"
date:                  2024-01-29T00:02:39.188977-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
