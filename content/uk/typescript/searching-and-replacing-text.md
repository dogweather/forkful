---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:59:03.859788-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/searching-and-replacing-text.md"
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