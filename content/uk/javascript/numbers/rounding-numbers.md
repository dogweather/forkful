---
title:                "Округлення чисел"
date:                  2024-01-26T03:45:51.257245-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення - це видалення "шуму" після певної точки в числі. Програмісти округлюють для контролю точності, управління пам'яттю або роблять виведення користувача простішим — наприклад, перетворюючи 2.998 на чисті 3.

## Як це зробити:
Ось як ви можете округлити числа в JavaScript за допомогою `Math.round()`, `Math.ceil()`, і `Math.floor()`: 

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (оскільки .567 більше ніж .5)

console.log(roundedDown); // Виводить: 2
console.log(roundedUp);   // Виводить: 3
console.log(rounded);     // Виводить: 3
```

Щоб встановити певну кількість десяткових місць, використовуйте `toFixed()`:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (повертає рядок)

console.log(twoDecimals); // Виводить: "2.57"
```

Поверніть рядок назад до числа за допомогою унарного плюсу або `Number()`:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // Виводить: 2.57
```

## Поглиблено
Округлення чисел не є новим; це настільки ж старо, як і числа. У JavaScript `Math.round()` використовує метод "округлення до найближчого парного" для розрішення ситуацій з 0.5: якщо дробова частина становить 0.5, він округляє до найближчого парного числа.

Для більшого контролю `toFixed()` може бути вашим вибором, але пам'ятайте, що вона повертає рядок. Повернення до числа може бути додатковим кроком, але забезпечує вашу роботу з числовими типами.

Альтернативи? Бібліотеки на кшталт `lodash` пропонують `_.round(number, [precision=0])` для більш точного контролю. Або новітній `Intl.NumberFormat` надає вам високоточне форматування, яке виходить за рамки простого округлення.

Говорячи про точність, будьте обережні з особливостями роботи з плаваючою точкою в JavaScript. `0.1 + 0.2` не точно дорівнює `0.3` через спосіб зберігання чисел. Іноді, округлення стає необхідним для виправлення таких помилок з плаваючою точкою.

## Дивіться також
- Документація Mozilla по Math: [MDN Веб-документація](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Math)
- Фінансове округлення за допомогою `Intl.NumberFormat`: [API міжнародної стандартизації ECMAScript](https://tc39.es/ecma402/#numberformat-objects)
- Округлення з `lodash`: [Документація Lodash](https://lodash.com/docs/4.17.15#round)
