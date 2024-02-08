---
title:                "Округлення чисел"
aliases:
- uk/typescript/rounding-numbers.md
date:                  2024-01-26T03:47:45.163774-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення чисел - це зведення числа до певної точності. Програмісти роблять це, щоб контролювати числовий вивід для читабельності, цілей відображення або коли після операцій, що дають результати з плаваючою крапкою, потрібна певна точність.

## Як:
Округлення в TypeScript можна виконати за допомогою кількох методів. Ось короткий огляд:

```typescript
// Math.round округлює до найближчого цілого числа
console.log(Math.round(1.5)); // Вивід: 2

// Math.ceil округлює вгору до найближчого цілого числа
console.log(Math.ceil(1.1)); // Вивід: 2

// Math.floor округлює вниз до найближчого цілого числа
console.log(Math.floor(1.8)); // Вивід: 1

// toFixed округлює до фіксованої кількості десяткових знаків
let num = 1.23456;
console.log(num.toFixed(2)); // Вивід: "1.23"
// Зауважте: toFixed повертає рядок! Використовуйте parseFloat, щоб конвертувати назад, якщо потрібно.
console.log(parseFloat(num.toFixed(2))); // Вивід: 1.23
```

## Поглиблений огляд
В старі часи округлення було необхідним через обмежений простір і проблеми з точністю на ранніх комп'ютерах. Сьогодні арифметика з плаваючою крапкою може призводити до дивних результатів через спосіб зберігання чисел у двійковому форматі. Альтернативи округленню включають floor, ceil та trunc (для видалення десяткових дробів без округлення).

Внутрішні механізми заслуговують на увагу: `Math.round` слідує принципу "округлення до більшого" (також відомий як "комерційне округлення"), тоді як `Math.floor` і `Math.ceil` є простими. `toFixed` може спричиняти неочікувані результати, оскільки він повертає рядок, і він округляє за принципом "округлення до найближчого парного" (також відомий як "округлення банкіра"), особливо корисне для зниження упередженості при багаторазовому округленні одних і тих самих чисел.

## Дивіться також
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE стандарт для арифметики з плаваючою крапкою (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
