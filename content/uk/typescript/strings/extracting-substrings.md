---
date: 2024-01-20 17:47:06.319823-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 TypeScript, \u044F\u043A \u0456 \u0432 JavaScript, \u0456\u0441\u0442\u043E\
  \u0440\u0438\u0447\u043D\u043E \u0456\u0441\u043D\u0443\u0454 \u0434\u0435\u043A\
  \u0456\u043B\u044C\u043A\u0430 \u043C\u0435\u0442\u043E\u0434\u0456\u0432 \u0434\
  \u043B\u044F \u0432\u0438\u0442\u044F\u0433\u0443 \u043F\u0456\u0434\u0440\u044F\
  \u0434\u043A\u0456\u0432, \u0430\u043B\u0435 \u043D\u0435 \u0432\u0441\u0456 \u043E\
  \u0434\u043D\u0430\u043A\u043E\u0432\u043E \u0433\u0430\u0440\u043D\u0456. \u041D\
  \u0430\u043F\u0440\u0438\u043A\u043B\u0430\u0434, `substr` \u0454\u2026"
lastmod: '2024-04-05T21:53:49.081299-06:00'
model: gpt-4-1106-preview
summary: "\u0423 TypeScript, \u044F\u043A \u0456 \u0432 JavaScript, \u0456\u0441\u0442\
  \u043E\u0440\u0438\u0447\u043D\u043E \u0456\u0441\u043D\u0443\u0454 \u0434\u0435\
  \u043A\u0456\u043B\u044C\u043A\u0430 \u043C\u0435\u0442\u043E\u0434\u0456\u0432\
  \ \u0434\u043B\u044F \u0432\u0438\u0442\u044F\u0433\u0443 \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432, \u0430\u043B\u0435 \u043D\u0435 \u0432\u0441\u0456\
  \ \u043E\u0434\u043D\u0430\u043A\u043E\u0432\u043E \u0433\u0430\u0440\u043D\u0456\
  ."
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## Як це зробити:
```TypeScript
// Використання slice
const fullString: string = 'Привіт, світ!';
const substring: string = fullString.slice(0, 7);
console.log(substring); // Виведе: Привіт,

// Використання substring
const anotherSubstring: string = fullString.substring(8, 12);
console.log(anotherSubstring); // Виведе: світ

// Використання substr (застарілий)
const oldSchoolSubstring: string = fullString.substr(8, 4);
console.log(oldSchoolSubstring); // Виведе: світ
```

## Поглиблений розгляд
У TypeScript, як і в JavaScript, історично існує декілька методів для витягу підрядків, але не всі однаково гарні. Наприклад, `substr` є застарілим, бо його робота може відрізнятись в різних виконавчих середовищах, тому краще використовувати `slice` або `substring`. Обидва методи викликаються на рядку і приймають індекси, що визначають початок і кінець необхідної частини. Різниця між `slice` і `substring` полягає у тому, що `slice` може приймати від'ємні індекси, обчислюючи їх від кінця рядка.

## Дивіться також
- [String.prototype.slice() | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [String.prototype.substring() | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
