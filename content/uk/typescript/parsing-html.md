---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Що таке & Навіщо?

Розбір HTML, або парсинг HTML, - це процес аналізу HTML-коду, щоб отримати структуровані дані й взаємодіяти з ними. Програмісти роблять це для автоматичної обробки, маніпуляції даними або витягування інформації з веб-сторінок.

## Як це зробити:

```TypeScript
import { parse } from 'node-html-parser';

const html = `<ul id="list"><li class="item">Item One</li><li class="item">Item Two</li></ul>`;
const root = parse(html);

console.log(root.querySelector("#list").text);  // "Item OneItem Two"
```

Вищенаведений код вихоплює текст з елементів у списку HTML.

## Пірнаємо глибше

Розбір HTML виник у 1990-их роках з появою перших веб-сторінок. Є багато альтернатив (бібліотек і модулів), щоб робити це, залежно від мови програмування. Зазвичай, вони будують DOM-дерево з HTML-коду, тобто модель сторінки, яку можна проаналізувати і змінити. Наприклад, в JavaScript можна використовувати JSDOM або Cheerio, а в Python - Beautiful Soup.

## Дивіться також 

1. [Node HTML Parser на GitHub](https://github.com/taoqf/node-html-parser)
2. [Стаття на MDN про DOM](https://developer.mozilla.org/uk/docs/Web/API/Document_Object_Model)
3. [Python Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
4. [JSDOM GitHub Repo](https://github.com/jsdom/jsdom)
5. [Cheerio на GitHub](https://github.com/cheeriojs/cheerio)