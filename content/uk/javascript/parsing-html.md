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

## Чому

Розбір HTML є важливою частиною розробки веб-додатків. Він дозволяє нам отримувати структуру інформації з HTML документу для подальшого використання в наших програмах.

## Як

### Використання розбору ланцюжків за допомогою `querySelectorAll`

```Javascript
const elements = document.querySelectorAll('tag-name');
console.log(elements);
```

Вихідний код міститиме масив об'єктів, які відповідають тегам з потрібним ім'ям.

### Використання бібліотеки Cheerio для серверного розбору HTML

```Javascript
const cheerio = require('cheerio');
const html = '<div><h1>Hello world</h1></div>';
const $ = cheerio.load(html);
console.log($('h1').text());
```

Вихідний результат буде містити текст "Hello world", який знаходиться всередині тега `<h1>`.

### Використання вбудованих методів DOM для розбору HTML

```Javascript
const element = document.getElementById('element-id');
console.log(element.innerHTML);
```

Вихідний результат буде містити HTML код, який належить елементу з вказаним ID.

## Глибока занурення

Розбір HTML може бути більш складним завданням, коли маємо справу зі складною структурою документу. У таких випадках, корисно ознайомитись з більш складними методами, такими як `XMLHttpRequest` або `fetch`, для отримання HTML коду із зовнішніх документів.

## Дивіться також

- MDN розділ про DOM: https://developer.mozilla.org/uk/docs/Web/API/Document_Object_Model
- Офіційна документація Cheerio: https://github.com/cheeriojs/cheerio
- Застосування розбору HTML у реальному веб-додатку: https://medium.com/@magnetikonline/responding-to-dynamic-dom-changes-when-scraping-ca3b20ad12d4