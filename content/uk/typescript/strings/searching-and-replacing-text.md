---
date: 2024-01-20 17:59:03.859788-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0443 \u2014 \u0434\u0430\u0432\u043D\u044F \u043F\
  \u043E\u0442\u0440\u0435\u0431\u0430, \u0449\u043E \u0434\u0430\u0442\u0443\u0454\
  \u0442\u044C\u0441\u044F \u043F\u0435\u0440\u0456\u043E\u0434\u043E\u043C \u043C\
  \u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0440\u0435\u0434\u0430\u0433\u0443\u0432\
  \u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u0456\u0432. \u0412 JavaScript\
  \ \u0456 TypeScript, `.replace()` \u043C\u0435\u0442\u043E\u0434\u2026"
lastmod: '2024-04-05T21:53:49.076570-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443 \u2014 \u0434\u0430\u0432\u043D\u044F\
  \ \u043F\u043E\u0442\u0440\u0435\u0431\u0430, \u0449\u043E \u0434\u0430\u0442\u0443\
  \u0454\u0442\u044C\u0441\u044F \u043F\u0435\u0440\u0456\u043E\u0434\u043E\u043C\
  \ \u043C\u0430\u0441\u043E\u0432\u043E\u0433\u043E \u0440\u0435\u0434\u0430\u0433\
  \u0443\u0432\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u0456\u0432."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## Як це зробити:
```TypeScript
function searchAndReplace(text: string, searchValue: string | RegExp, replaceValue: string): string {
  return text.replace(searchValue, replaceValue);
}

// Використання:
const originalText = 'Привіт, світ!';
const newText = searchAndReplace(originalText, 'світ', 'галактика');
console.log(newText); // Виведе: Привіт, галактика!
```

## Поглиблений аналіз:
Пошук та заміна тексту — давня потреба, що датується періодом масового редагування текстів. В JavaScript і TypeScript, `.replace()` метод можна використовувати зі строками або регулярними виразами для гнучкості пошуку. Альтернативою є бібліотеки, як-от Lodash, що надають додаткові утиліти для роботи з текстами. Розробники повинні розуміти, як використовувати g-flag (глобальний пошук) для заміни всіх входжень, і не забувати про безпеку при роботі з динамічно вводимими регулярними виразами.

## Дивіться також:
- MDN RegExp документація: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- TypeScript Handbook String Manipulation: [https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Lodash бібліотека: [https://lodash.com/](https://lodash.com/)
