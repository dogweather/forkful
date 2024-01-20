---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Завантаження веб-сторінки - це процес отримання інформації з веб-сайту. Програмісти роблять це, щоб аналізувати структуру сайтів, отримувати потрібні дані або створювати резервні копії.

## Як це зробити:

Ось базовий приклад кода для завантаження веб-сторінки в Javascript за допомогою модуля `axios`.

```Javascript
const axios = require('axios');

axios.get('https://example.com')
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.log(error);
  });
```
Вихідний код повинен відображати HTML структуру запитуваної веб-сторінки.

## Поглиблений дискурс:

Історично завантаження веб-сторінок було складним процесом, який вимагав глибоких знань мережевого рівня. З появою Node.js і модулів, таких як `axios`, завантаження стало набагато простішим.

Альтернативою `axios` може служити модуль `request`. Він також дозволяє завантажувати веб-сторінки, але має інший API.

Ці модулі працюють, відправляючи HTTP GET запит до сервера, який розміщує веб-сайт. Сервер повертає відповідь, яку модуль інтерпретує як HTML сторінку.

## Див. також:

- [HTTP - Вікіпедія](https://uk.wikipedia.org/wiki/HTTP)
- [Axios](https://www.npmjs.com/package/axios)
- [Request](https://www.npmjs.com/package/request)