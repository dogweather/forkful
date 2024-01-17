---
title:                "Аналізування HTML"
html_title:           "TypeScript: Аналізування HTML"
simple_title:         "Аналізування HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-html.md"
---

{{< edit_this_page >}}

# Що і чому?
Parsing HTML - це процес аналізування інформації, що міститься у веб-сторінках. Це часто робиться програмістами для отримання потрібної інформації з веб-сайту для подальшого використання.

# Як:
```TypeScript
// Приклад парсингу HTML за допомогою бібліотеки Cheerio
import * as cheerio from 'cheerio';

const html = '<ul><li>Пункт 1</li><li>Пункт 2</li><li>Пункт 3</li></ul>';
const $ = cheerio.load(html); // ініціалізація об’єкта Cheerio

$('li').each((index, element) => { // знаходимо всі елементи <li> та виконуємо функцію для кожного
    console.log($(element).text()); // виводимо вміст кожного елементу
});

// Результат:
// Пункт 1
// Пункт 2
// Пункт 3
```

# Глибокий погляд:
Parsing HTML був особливо актуальним у минулому, коли браузери не могли коректно відображати веб-сторінки без наявності вбудованого парсера. Сьогодні є також альтернативні способи отримання потрібної інформації з веб-сайту, такі як використання API або регулярних виразів. У TypeScript, на додаток до використання бібліотеки Cheerio, можливо використовувати інші бібліотеки, наприклад jsdom, для парсингу HTML.

# Подивитися також:
- [Документація по бібліотеці Cheerio](https://github.com/cheeriojs/cheerio)
- [Інструкція для використання jsdom](https://www.npmjs.com/package/jsdom)
- [Корисна стаття на тему парсингу HTML за допомогою TypeScript](https://blog.bitsrc.io/web-scraping-with-node-js-c93dcf76fe2b)