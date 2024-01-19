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

## Що таке та навіщо?

Парсінг HTML - це процес видобування даних із HTML документу. Програмісти роблять це для перетворення структурованого хтмл в код щоб легко доступитися до його компонентів.

## Як це зробити:

Можна використовувати Node.JS з JSDOM для парсингу HTML. Ось код, який ви можете спробувати:

```javascript
var jsdom = require("jsdom");
var { JSDOM } = jsdom;
var dom = new JSDOM(` 
  <!DOCTYPE html>
  <div class="hello-world">Привіт, світ! </div>`);
console.log(dom.window.document.querySelector(".hello-world").textContent); // "Привіт, світ!"
```
## Поглиблений огляд:

1. **Історичний контекст:** Парсинг HTML з'явився з розвитком вебу. Він покращив якість взаємодії користувачів з інтернетом.

2. **Альтернативи:** Інші варіанти парсингу HTML включають бібліотеки, такі як "Beautiful Soup" (Python) або "Cheerio" (Node.js).

3. **Деталі реалізації:** Під час парсингу HTML, код програми читає файл HTML та перетворює його на дерево об'єктів, зазвичай DOM.

## Див. також:

1. [JSDOM Documentation](https://github.com/jsdom/jsdom)
2. [Cheerio Documentation](https://cheerio.js.org/)
3. [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)