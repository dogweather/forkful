---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:59.957944-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0427\u0442\u043E\u0431\u044B \u043D\u0430\u0447\u0430\u0442\u044C\
  , \u0443\u0441\u0442\u0430\u043D\u043E\u0432\u0438\u0442\u0435 \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0443, \u043D\u0430\u043F\u0440\u0438\u043C\
  \u0435\u0440, `node-html-parser`. \u0412\u043E\u0442 \u043A\u043E\u043C\u0430\u043D\
  \u0434\u0430 \u0434\u043B\u044F \u0442\u0435\u0440\u043C\u0438\u043D\u0430\u043B\
  \u0430."
lastmod: '2024-03-13T22:44:44.585121-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u043D\u0430\u0447\u0430\u0442\u044C, \u0443\
  \u0441\u0442\u0430\u043D\u043E\u0432\u0438\u0442\u0435 \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0443, \u043D\u0430\u043F\u0440\u0438\u043C\u0435\u0440\
  , `node-html-parser`."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
Чтобы начать, установите библиотеку, например, `node-html-parser`. Вот команда для терминала:

```bash
npm install node-html-parser
```

Теперь давайте разберем некоторый базовый HTML в TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Яблоко</li>
                <li>Банан</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Яблоко Банан"
```

И если вы хотите выбрать только бананы:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Банан"
```

## Глубокое Погружение
Разбор HTML не новость – он существует с начала дней веба. Изначально разработчики могли использовать регулярные выражения, но это быстро становилось запутанным. Вот тут-то и появился DOM Parser: стабильный, но ограниченный браузером.

Библиотеки вроде `node-html-parser` избавляют от боли. Они позволяют вам запрашивать HTML, как вы бы сделали это с jQuery, но на стороне сервера с Node.js. Это быстро, терпимо к "грязному" HTML и дружелюбно к DOM.

Есть также `jsdom`, симулирующий целое браузерное окружение. Оно тяжелее, но более тщательное, создавая полноценную Документную Объектную Модель (DOM) для манипуляции и взаимодействия.

Не забудем и про Cheerio. Она сочетает в себе скорость с синтаксисом, похожим на jQuery, и меньшим объемом, удобно располагаясь между двумя другими.

## Смотрите Также
Если вам интересно узнать больше, загляните сюда:
- [Спецификация W3C по Разбору и Сериализации DOM](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser на GitHub](https://github.com/taoqf/node-html-parser)
- [Репозиторий jsdom на GitHub](https://github.com/jsdom/jsdom)
- [Веб-сайт Cheerio](https://cheerio.js.org/)
