---
date: 2024-01-20 17:59:03.859788-07:00
description: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\
  \u043D\u0430 \u0442\u0435\u043A\u0441\u0442\u0443 - \u0446\u0435 \u043F\u0440\u043E\
  \u0446\u0435\u0441 \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F\
  \ \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\u0456\u0432 \u0441\u0442\u0440\
  \u043E\u043A \u0456 \u0457\u0445 \u0437\u0430\u043C\u0456\u043D\u0438 \u043D\u0430\
  \ \u0456\u043D\u0448\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0448\u0432\u0438\u0434\u043A\u043E\
  \ \u0437\u043C\u0456\u043D\u044E\u0432\u0430\u0442\u0438 \u043A\u043E\u0434 \u0430\
  \u0431\u043E \u0434\u0430\u043D\u0456,\u2026"
lastmod: '2024-03-11T00:14:22.680447-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443 - \u0446\u0435 \u043F\u0440\u043E\u0446\
  \u0435\u0441 \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F\
  \ \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\u0456\u0432 \u0441\u0442\u0440\
  \u043E\u043A \u0456 \u0457\u0445 \u0437\u0430\u043C\u0456\u043D\u0438 \u043D\u0430\
  \ \u0456\u043D\u0448\u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\
  \u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u044E\
  \u0442\u044C \u0446\u0435, \u0449\u043E\u0431 \u0448\u0432\u0438\u0434\u043A\u043E\
  \ \u0437\u043C\u0456\u043D\u044E\u0432\u0430\u0442\u0438 \u043A\u043E\u0434 \u0430\
  \u0431\u043E \u0434\u0430\u043D\u0456,\u2026"
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
---

{{< edit_this_page >}}

## Що це та навіщо?
Пошук та заміна тексту - це процес знаходження фрагментів строк і їх заміни на інші. Програмісти використовують це, щоб швидко змінювати код або дані, виправляти помилки, оновлювати інформацію чи рефакторинг.

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
