---
title:                "Аналіз HTML"
aliases:
- /uk/typescript/parsing-html/
date:                  2024-02-03T19:13:35.618714-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Парсинг HTML означає відсіювання коду HTML для пошуку, вилучення або маніпуляції інформацією. Програмісти роблять це для взаємодії з веб-контентом—можливо, для скрапінгу даних чи автоматизації браузерів.

## Як робити:

Щоб розпочати, встановіть бібліотеку, як от `node-html-parser`. Ось команда для терміналу:

```bash
npm install node-html-parser
```

Тепер давайте розберемо деякий базовий HTML в TypeScript:

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

А якщо ви хочете отримати лише банани:

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## Глибше занурення

Парсинг HTML не є чимось новим—він існує з ранніх днів вебу. Спочатку розробники могли користуватися регулярними виразами, але це швидко стало заплутаним. Введемо DOM Parser: стабільний, але прив'язаний до браузера.

Бібліотеки на кшталт `node-html-parser` абстрагують цей біль. Вони дозволяють запитувати HTML так, як це можна зробити з jQuery, але на стороні сервера з Node.js. Це швидко, толерантно до нечистого HTML і дружнє до DOM.

Є також `jsdom`, який симулює ціле браузерне середовище. Він важчий, але більш повний, створюючи повноцінну модель об’єкта документа (DOM) для маніпуляції та взаємодії.

Не забудьмо також про Cheerio. Воно поєднує швидкість з синтаксисом подібним до jQuery і меншим розміром, займаючи щасливе місце між двома.

## Див. також

Якщо ви хочете дізнатися більше, загляньте сюди:
- [Специфікація W3C з парсингу та серіалізації DOM](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser на GitHub](https://github.com/taoqf/node-html-parser)
- [Репозиторій jsdom на GitHub](https://github.com/jsdom/jsdom)
- [Вебсайт Cheerio](https://cheerio.js.org/)
