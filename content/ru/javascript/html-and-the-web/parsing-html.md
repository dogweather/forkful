---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:53.818102-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0437\u0431\
  \u0435\u0440\u0435\u043C HTML \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ API `DOMParser` \u0432 JavaScript."
lastmod: '2024-03-13T22:44:45.756507-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0440\u0430\u0437\u0431\u0435\
  \u0440\u0435\u043C HTML \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E API `DOMParser`\
  \ \u0432 JavaScript."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Давайте разберем HTML с помощью API `DOMParser` в JavaScript.

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Привет, мир!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // Вывод: Привет, мир!
```

Теперь давайте получим что-то более конкретное, например, элемент с классом:

```Javascript
const htmlString = `<div><p class="greeting">Привет, снова!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // Вывод: Привет, снова!
```

## Подробнее
Парсинг HTML существует столько же, сколько и сам веб. Изначально это была задача браузеров — они разбирали HTML, чтобы отображать веб-страницы. Со временем программисты захотели вмешаться в этот процесс, что привело к появлению API вроде `DOMParser`.

Альтернативы? Конечно. У нас есть библиотеки вроде `jQuery` и инструменты вроде `BeautifulSoup` для Python. Но нативный `DOMParser` JavaScript быстр и встроен, не требуется дополнительных библиотек.

С точки зрения реализации, когда вы парсите HTML с `DOMParser`, он создает объект `Document`. Воспринимайте его как иерархическую модель вашего HTML. Как только у вас он есть, вы можете навигировать и манипулировать им так же, как и с обычным DOM веб-страницы.

Вот в чем дело — парсинг может споткнуться о неправильно сформированный HTML. Браузеры прощают ошибки, но `DOMParser` может быть не таким снисходительным. Поэтому для сложных задач или беспорядочного HTML сторонние библиотеки могут сделать лучшую очистку.

## Смотрите также
- Документация MDN по API `DOMParser`: [MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- Возможности парсинга в jQuery: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, быстрая, гибкая и легковесная реализация основных функций jQuery для сервера: [Cheerio.js](https://cheerio.js.org/)
- Для парсинга не на JS: библиотека BeautifulSoup для Python: [Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
