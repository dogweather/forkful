---
title:                "Парсинг HTML"
date:                  2024-01-20T15:35:04.325863-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"

category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Парсинг HTML — це процес аналізу HTML-коду для отримання даних чи структури сторінки. Програмісти парсять HTML, щоб маніпулювати контентом, автоматизувати роботу з веб-формами та збирати інформацію з веб-сайтів.

## Як це зробити:

Створимо простий парсер HTML у TypeScript за допомогою бібліотеки `node-html-parser`. Найперше, встановимо бібліотеку:

```shell
npm install node-html-parser
```

Тепер застосуємо її у коді:

```typescript
import { parse } from 'node-html-parser';

const htmlContent = `
  <html>
    <body>
      <p>Привіт, світ!</p>
    </body>
  </html>
`;

const root = parse(htmlContent);
const p = root.querySelector('p');
console.log(p?.textContent); // Виведе 'Привіт, світ!'
```

## Поглиблено:

HTML парсинг — не новий тренд. З початком розвитку вебу програмісти шукали способи читати та керувати HTML. До популярності JavaScript та серверних мов, таких як PHP чи Ruby, HTML зазвичай парсився з допомогою регулярних виразів, що нині вважається поганою практикою через складність надійного аналізу.

Сьогодні, крім `node-html-parser`, існує безліч бібліотек, наприклад, `cheerio` чи `jsdom`, які можуть емулювати поведінку браузера для більш глибокої взаємодії з DOM.

Парсер має відповідати стандартам, що означає працювати з некоректним HTML, аналогічно як це роблять браузери. Процес парсингу складний та включає токенізацію, відстеження стану та відновлення помилок.

## Додатково:

- Бібліотека `node-html-parser`: https://www.npmjs.com/package/node-html-parser
- Про `cheerio`: https://cheerio.js.org/
- Про `jsdom`: https://github.com/jsdom/jsdom
- Специфікація HTML парсингу: https://html.spec.whatwg.org/
