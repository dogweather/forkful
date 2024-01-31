---
title:                "Округление чисел"
date:                  2024-01-29T00:02:08.879836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"

category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Округление — это отсечение лишнего после определенной точки в числе. Программисты округляют, чтобы контролировать точность, управлять памятью или сделать вывод понятнее для пользователя — например, превратить 2.998 в чистую тройку.

## Как это делать:
Вот как округлять числа в JavaScript с помощью `Math.round()`, `Math.ceil()` и `Math.floor()`:

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (поскольку .567 больше чем .5)

console.log(roundedDown); // Выводит: 2
console.log(roundedUp);   // Выводит: 3
console.log(rounded);     // Выводит: 3
```

Чтобы зафиксировать определенное количество десятичных мест, используйте `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (возвращает строку)

console.log(twoDecimals); // Выводит: "2.57"
```

Чтобы преобразовать строку обратно в число, используйте унарный плюс или `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Выводит: 2.57
```

## Подробнее
Округление чисел не ново; оно так же старо, как и числа. В JavaScript `Math.round()` использует метод "арифметического округления": если дробная часть равна 0.5, округляет к ближайшему четному числу.

Для большего контроля `toFixed()` может быть вашим вариантом, но помните, что он возвращает строку. Преобразование обратно в число может быть дополнительным шагом, но это гарантирует, что вы продолжите работать с числовыми типами.

Альтернативы? Библиотеки вроде `lodash` предлагают `_.round(number, [precision=0])` для более тонкого контроля. Или новейший `Intl.NumberFormat` дает вам высокоточную форматирование не только для округления.

Говоря о точности, стоит остерегаться особенностей с плавающей точкой в JavaScript. `0.1 + 0.2` не совсем равно `0.3` из-за того, как хранятся числа. Иногда округление становится необходимым, чтобы исправить такие ошибки с плавающей точкой.

## Смотрите также
- Документация по Math от Mozilla: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Округление для финансовых расчетов с `Intl.NumberFormat`: [API интернационализации ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Округление `lodash`: [Документация Lodash](https://lodash.com/docs/4.17.15#round)
