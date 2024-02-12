---
title:                "Разбор HTML"
aliases:
- /ru/typescript/parsing-html.md
date:                  2024-01-28T23:59:59.957944-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Разбор HTML означает просеивание кода HTML для поиска, извлечения или манипуляции информацией. Программисты делают это для взаимодействия с веб-контентом – возможно, для сбора данных или автоматизации браузеров.

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
