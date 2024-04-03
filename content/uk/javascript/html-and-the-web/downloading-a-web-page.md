---
date: 2024-01-20 17:44:32.477370-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : JavaScript \u043C\u043E\u0436\u0435 \u0437\u0430\u0432\u0430\u043D\u0442\u0430\
  \u0436\u0438\u0442\u0438 \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\
  \u043A\u0443 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ API Fetch \u0430\u0431\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \u0438, \u044F\u043A-\u043E\u0442 `axios`. \u041E\u0441\u044C \u043F\u0440\u043E\
  \u0441\u0442\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434 \u0456\u0437\
  \ Fetch."
lastmod: '2024-03-13T22:44:49.991228-06:00'
model: gpt-4-1106-preview
summary: "JavaScript \u043C\u043E\u0436\u0435 \u0437\u0430\u0432\u0430\u043D\u0442\
  \u0430\u0436\u0438\u0442\u0438 \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\
  \u043D\u043A\u0443 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E API Fetch \u0430\u0431\u043E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0438, \u044F\u043A-\u043E\u0442 `axios`."
title: "\u0417\u0430\u0432\u0430\u043D\u0442\u0430\u0436\u0435\u043D\u043D\u044F \u0432\
  \u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0438"
weight: 42
---

## Як це зробити:
JavaScript може завантажити веб-сторінку за допомогою API Fetch або бібліотеки, як-от `axios`. Ось простий приклад із Fetch:

```javascript
fetch('https://some-website.com')
  .then(response => {
    if (!response.ok) {
      throw new Error('Network response was not OK');
    }
    return response.text();
  })
  .then(html => {
    console.log(html);
  })
  .catch(err => {
    console.error('Failed to fetch page: ', err);
  });
```

Цей код друкує HTML веб-сторінки у консоль.

## Поглиблений розбір:
У минулому для завантаження веб-сторінок використовували XMLHTTPRequest. Але Fetch API - це більш сучасний і простий спосіб, який забезпечує проміси. Альтернативно, Node.js має свої модулі, такі як `http` і `https` для серверного запиту.

Завантаження веб-сторінки інколи називають "скрейпінгом". Це може включати аналіз HTML і запис даних. Обережно з Термінами Використання сайтів, деякі забороняють скрейпінг.

## Дивись також:
- [MDN Web Docs - Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Axios on GitHub](https://github.com/axios/axios)
- [Node.js `http` module documentation](https://nodejs.org/api/http.html)

Ці ресурси допоможуть розширити ваші знання і дадуть більше інструментів для завантаження веб-сторінок.
