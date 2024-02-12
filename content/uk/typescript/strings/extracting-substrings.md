---
title:                "Виділення підрядків"
aliases:
- /uk/typescript/extracting-substrings.md
date:                  2024-01-20T17:47:06.319823-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке & навіщо?
Витягнення підрядків – це процес отримання частин із більших рядків. Це корисно для аналізу тексту, форматування даних або коли потрібно працювати лише з частиною інформації.

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
