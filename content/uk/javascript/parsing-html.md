---
title:                "Javascript: Аналіз HTML"
simple_title:         "Аналіз HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Чому

Кожен, хто працює з веб-розробкою, будь-яким чином пов'язаному з обробкою HTML-коду, повинен легко розбиратися в аналізі цього типу даних. Здатність до парсингу HTML дозволяє отримувати необхідну інформацію з веб-сторінок, що дуже корисно, коли потрібно отримати цілісну картину про цільову аудиторію або виконувати аналіз конкурентів.

## Як це зробити

Для початку потрібно завантажити документ HTML у вигляді рядка. Для цього можна використовувати бібліотеку `axios`:

```javascript
const axios = require('axios');

axios.get('https://example.com')
    .then((response) => {
        const htmlString = response.data;
        // Використовуємо htmlString для подальшого парсингу HTML-коду
    })
    .catch((error) => {
        console.log(error);
    });
```
Тепер, коли ми маємо HTML у вигляді рядка, можемо починати його аналіз. Для цього можна використовувати бібліотеку `cheerio`, яка дозволяє використовувати синтаксис селекторів CSS для отримання необхідних елементів на сторінці:

```javascript
const cheerio = require('cheerio');

const $ = cheerio.load(htmlString);
// Парсимо заголовок з тегом <h1>
const heading = $('h1').text();
console.log(heading); // Output: Example Domain
```

## Profundo in HTML

Існує багато інших способів отримати необхідну інформацію з HTML-коду. Наприклад, для отримання вмісту з тегів `<meta>` можна використовувати бібліотеку `metascraper`:

```javascript
const metascraper = require('metascraper')([
    require('metascraper-description')()
]);

// Парсимо заголовок з тегом <meta name="description">
metascraper({ html: htmlString })
    .then((metadata) => {
        const description = metadata.description;
        console.log(description); // Output: Example Domain
    });
```

Є також можливість використовувати бібліотеки для парсингу конкретних типів даних, наприклад `rss-parser` для отримання оновлень з RSS-стрічок.

## Дивись також

Детальніше про парсинг HTML можна дізнатися у документації бібліотек, зазначених у цій статті:

- [axios](https://github.com/axios/axios)
- [cheerio](https://github.com/cheeriojs/cheerio)
- [metascraper](https://github.com/microlinkhq/metascraper)
- [rss-parser](https://github.com/rbren/rss-parser)