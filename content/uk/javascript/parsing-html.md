---
title:                "Парсинг HTML"
date:                  2024-01-20T15:32:51.759876-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
HTML-парсинг — це процес аналізу HTML-коду для отримання даних, що його структурують. Робимо це, щоб автоматизувати роботу з даними, які спочатку призначені для людського читання.

## Як це зробити:
```Javascript
// Використання DOMParser для парсингу HTML
const parser = new DOMParser();
const htmlString = '<div>Hello, world!</div>';
const doc = parser.parseFromString(htmlString, 'text/html');

console.log(doc.body.textContent); // Вивід: Hello, world!
```

```Javascript
// Використання бібліотеки jQuery для шукання елементів
let $html = $(htmlString);
let text = $html.find('div').text();

console.log(text); // Вивід: Hello, world!
```

## Поглиблено:
HTML-парсинг — не новинка. Спочатку в основному використовували регулярні вирази, але це може призвести до помилок. Точність та зручність покращилися з введенням DOMParser.

Є й інші методи, як наприклад: інтерфейс `innerHTML` або бібліотеки як jQuery. На великих проєктах можуть бути використані серверні рішення: наприклад, Node.js з бібліотеками типу `cheerio`.

При роботі з парсингом HTML, важливо розуміти безпечність. Вхідний HTML може містити шкідливий код, переконайтесь у використанні надійних інструментів та чищенні контенту перед взаємодією.

## Додатково:
- [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- [Cheerio.js](https://cheerio.js.org/)
- [Техніки безпечного парсингу HTML](https://owasp.org/www-community/attacks/xss/) (OWASP)
