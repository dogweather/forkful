---
title:                "Розбір html"
html_title:           "Javascript: Розбір html"
simple_title:         "Розбір html"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-html.md"
---

{{< edit_this_page >}}

Що і чому?

Парсинг HTML є процесом видобування даних з різноманітних веб-сайтів. Програмісти використовують цей процес, щоб автоматизувати збір необхідної інформації з веб-сторінок, що допомагає скоротити час та зусилля при роботі з великим обсягом даних.

Як це зробити:
```Javascript
const request = require('request');
const cheerio = require('cheerio');

// Використовуємо модуль request для отримання коду сторінки
request('https://example.com', (error, response, html) => {
    if (!error && response.statusCode == 200) {
        // Використовуємо cheerio для отримання доступу до DOM сторінки
        const $ = cheerio.load(html);

        // Знаходимо елемент за допомогою CSS селектора та отримуємо його текст
        const title = $('h1').text();

        // Виводимо результат
        console.log(title);
    }
});
```

Глибше вдивимося:
(1) Історичний контекст: Парсинг HTML був розроблений для автоматизації збору даних з веб-сторінок, що спрощує життя програмістам.
(2) Альтернативи: Замість використання бібліотеки cheerio, можна також використати Puppeteer або JSDOM для парсингу веб-сторінок.
(3) Деталі виконання: Після отримання HTML-коду сторінки, бібліотека cheerio дозволяє шукати елементи за допомогою CSS селекторів та отримувати потрібну інформацію.

Дивіться також:
- [Модуль request] (https://www.npmjs.com/package/request)
- [Бібліотека cheerio] (https://cheerio.js.org/)
- [Підходи до парсингу веб-сторінок у Node.js] (https://hackernoon.com/a-guide-to-web-scraping-in-node-js/)