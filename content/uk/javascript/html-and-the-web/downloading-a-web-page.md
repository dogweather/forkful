---
title:                "Завантаження веб-сторінки"
aliases:
- /uk/javascript/downloading-a-web-page/
date:                  2024-01-20T17:44:32.477370-07:00
model:                 gpt-4-1106-preview
simple_title:         "Завантаження веб-сторінки"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і чому?

Завантаження веб-сторінки - це отримання її копії з сервера. Програмісти роблять це, щоб аналізувати контент, виконувати автоматизацію чи просто зберегти дані локально.

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
