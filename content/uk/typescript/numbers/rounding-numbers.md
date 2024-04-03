---
date: 2024-01-26 03:47:45.163774-07:00
description: "\u042F\u043A: \u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\
  \u044F \u0432 TypeScript \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\
  \u043D\u0430\u0442\u0438 \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\
  \u043E\u044E \u043A\u0456\u043B\u044C\u043A\u043E\u0445 \u043C\u0435\u0442\u043E\
  \u0434\u0456\u0432. \u041E\u0441\u044C \u043A\u043E\u0440\u043E\u0442\u043A\u0438\
  \u0439 \u043E\u0433\u043B\u044F\u0434."
lastmod: '2024-03-13T22:44:48.860754-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0432 TypeScript\
  \ \u043C\u043E\u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u043D\u0430\u0442\u0438\
  \ \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u043A\u0456\
  \u043B\u044C\u043A\u043E\u0445 \u043C\u0435\u0442\u043E\u0434\u0456\u0432."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

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
