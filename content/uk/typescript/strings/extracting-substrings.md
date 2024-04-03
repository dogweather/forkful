---
date: 2024-01-20 17:47:06.319823-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:48.850940-06:00'
model: gpt-4-1106-preview
summary: .
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
