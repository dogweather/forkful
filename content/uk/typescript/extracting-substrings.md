---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і навіщо?

Видобування підрядків - це процес отримання менших рядків з більшого рядка. Розробники роблять це для аналізу та маніпулювання специфічними даними в межах більшого рядка. 

## Як це робити:

Подивимося на декілька прикладів в TypeScript:

```TypeScript
let str = "Привіт світ";
let subStr = str.substring(0, 6); 
console.log(subStr); // виводить: "Привіт"
```
У цьому прикладі, `substring()` використовується для отримання підрядка з індексу 0 до 6. 

```TypeScript
let str = "Привіт світ";
let subStr = str.substring(7); 
console.log(subStr); // виводить: "світ"
```
Тут `substring()` з одним аргументом повертає все з певного індексу і до кінця рядка.

## Поглиблений занурення:

Видобування підрядків існує в ком'ютерних науках стільки ж, скільки і саме програмування. Було декілька способів вирішення цієї проблеми в минулому,  включаючи функції, аналогічні `substring()`, але і `slice()`, `substr()` в JavaScript.

Як варіант, в TypeScript ви також можете використовувати `slice()`, який працює майже так само, але із деякими розходженнями: `substring()` не може приймати від'ємні індекси, тоді як `slice()` може.

Щодо реалізації, TypeScript працює як надбудова над JavaScript. Тому видобування підрядків в TypeScript працює так само, як і в JavaScript.

## Дивіться також:

1. [MDN Web Docs - String.prototype.substring()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
2. [MDN Web Docs - String.prototype.slice()](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
3. [TypeScript - String](https://www.typescripttutorial.net/typescript-tutorial/typescript-string/)
  
Звичайно, це тільки поверхня того, що ви можете робити із рядками в TypeScript. Продовжуйте вчитися та експериментуйте!