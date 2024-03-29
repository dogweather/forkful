---
date: 2024-01-26 03:47:45.163774-07:00
description: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\
  \u0441\u0435\u043B - \u0446\u0435 \u0437\u0432\u0435\u0434\u0435\u043D\u043D\u044F\
  \ \u0447\u0438\u0441\u043B\u0430 \u0434\u043E \u043F\u0435\u0432\u043D\u043E\u0457\
  \ \u0442\u043E\u0447\u043D\u043E\u0441\u0442\u0456. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\
  \ \u0446\u0435, \u0449\u043E\u0431 \u043A\u043E\u043D\u0442\u0440\u043E\u043B\u044E\
  \u0432\u0430\u0442\u0438 \u0447\u0438\u0441\u043B\u043E\u0432\u0438\u0439 \u0432\
  \u0438\u0432\u0456\u0434 \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u0431\u0435\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0456, \u0446\u0456\u043B\u0435\u0439 \u0432\
  \u0456\u0434\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F \u0430\u0431\u043E\
  \u2026"
lastmod: '2024-03-13T22:44:48.860754-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\
  \u0441\u0435\u043B - \u0446\u0435 \u0437\u0432\u0435\u0434\u0435\u043D\u043D\u044F\
  \ \u0447\u0438\u0441\u043B\u0430 \u0434\u043E \u043F\u0435\u0432\u043D\u043E\u0457\
  \ \u0442\u043E\u0447\u043D\u043E\u0441\u0442\u0456. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C\
  \ \u0446\u0435, \u0449\u043E\u0431 \u043A\u043E\u043D\u0442\u0440\u043E\u043B\u044E\
  \u0432\u0430\u0442\u0438 \u0447\u0438\u0441\u043B\u043E\u0432\u0438\u0439 \u0432\
  \u0438\u0432\u0456\u0434 \u0434\u043B\u044F \u0447\u0438\u0442\u0430\u0431\u0435\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0456, \u0446\u0456\u043B\u0435\u0439 \u0432\
  \u0456\u0434\u043E\u0431\u0440\u0430\u0436\u0435\u043D\u043D\u044F \u0430\u0431\u043E\
  \u2026"
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
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
