---
title:                "Розбір HTML"
aliases:
- /uk/javascript/parsing-html/
date:                  2024-01-28T03:01:03.758184-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/parsing-html.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Парсинг HTML означає витягування даних із документів HTML. Програмісти роблять це для взаємодії з веб-контентом або його маніпуляції, автоматизації витягування даних або для цілей веб-скрапінгу.

## Як:
Давайте спарсимо HTML використовуючи API `DOMParser` в JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Вивід: Hello, world!
```

Тепер давайте заберемо щось більш конкретне, як-от елемент з класом:

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Вивід: Hello, again!
```

## Поглиблено
Парсинг HTML є старим як веб. Спочатку це було справою браузерів — браузери парсили HTML для відображення веб-сторінок. З часом програмісти захотіли долучитися до цього процесу, що призвело до появи API, таких як `DOMParser`.

Альтернативи? Звичайно. У нас є бібліотеки на кшталт `jQuery` та інструменти на кшталт `BeautifulSoup` для Python. Але нативний `DOMParser` в JavaScript швидкий і вбудований, тому немає потреби в додаткових бібліотеках.

З точки зору реалізації, коли ви парсите HTML з `DOMParser`, це створює об'єкт `Document`. Думайте про це як про ієрархічну модель вашого HTML. Як тільки ви її отримаєте, ви можете навігувати та маніпулювати нею так само, як і з DOM звичайної веб-сторінки.

Ось що—парсинг може заплутатися на некоректному HTML. Браузери прощають, але `DOMParser` може і не пробачити. Отже, для складних завдань або заплутаного HTML сторонні бібліотеки можуть зробити кращу роботу з очистки.

## Дивіться також
- MDN Web Docs про API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- Відомості про парсинг в jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, швидка, гнучка та лаконічна реалізація основи jQuery для сервера: [Cheerio.js](https://cheerio.js.org/)
- Для нон-JS парсингу: бібліотека BeautifulSoup Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
