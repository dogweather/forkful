---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо це потрібно?

Завантаження веб-сторінки - це процес отримання даних від сервера до комп'ютера користувача. Програмісти роблять це, щоб аналізувати й обробляти цей вміст, здійснювати веб-скрапінг або автоматизувати деякі тести.

## Як це зробити:

Цей код TypeScript показує, як ви можете завантажити веб-сторінку за допомогою бібліотеки axios. 

```TypeScript
import axios from 'axios';

axios.get('http://example.com')
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.error(error);
  });
```

Коли ви запустите цей код, ви побачите вміст сторінки example.com у консолі.

## Поглиблений аналіз:

Завантаження веб-сторінок є старим як сам Інтернет. З вихідом першого браузера, було необхідно отримати дані з сервера. З того часу методи та технології дещо змінились.
Є декілька альтернатив axios для завантаження веб-сторінки, включаючи fetch() API або бібліотеку request у Node.js.
Axios відрізняється тим, що вона працює і в браузері, і в Node.js, підтримує обіцянки (Promises) і може перехоплювати запроси та відповіді.

## Дивіться також:

- [Axios документація](https://axios-http.com/docs/intro)
- [Fetch API документація](https://developer.mozilla.org/uk/docs/Web/API/Fetch_API/Using_Fetch)
- [Request бібліотека у Node.js](https://www.npmjs.com/package/request)
  
Нагадую, що завжди важливо знати, як і куди використовувати необхідні інструменти. Продовжуйте вчитися та розвиватися в своїх програмістських навиках!