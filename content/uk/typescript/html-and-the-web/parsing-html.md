---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:35.618714-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u0429\u043E\u0431\
  \ \u0440\u043E\u0437\u043F\u043E\u0447\u0430\u0442\u0438, \u0432\u0441\u0442\u0430\
  \u043D\u043E\u0432\u0456\u0442\u044C \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0443, \u044F\u043A \u043E\u0442 `node-html-parser`. \u041E\u0441\u044C \u043A\
  \u043E\u043C\u0430\u043D\u0434\u0430 \u0434\u043B\u044F \u0442\u0435\u0440\u043C\
  \u0456\u043D\u0430\u043B\u0443."
lastmod: '2024-03-13T22:44:48.865786-06:00'
model: gpt-4-0125-preview
summary: "\u0429\u043E\u0431 \u0440\u043E\u0437\u043F\u043E\u0447\u0430\u0442\u0438\
  , \u0432\u0441\u0442\u0430\u043D\u043E\u0432\u0456\u0442\u044C \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0443, \u044F\u043A \u043E\u0442 `node-html-parser`."
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
weight: 43
---

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
