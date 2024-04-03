---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:18.121131-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 TypeScript \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\
  \u0438\u0435 \u043C\u043E\u0436\u043D\u043E \u0432\u044B\u043F\u043E\u043B\u043D\
  \u0438\u0442\u044C \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u0438\u043C\u0438\
  \ \u0441\u043F\u043E\u0441\u043E\u0431\u0430\u043C\u0438. \u0412\u043E\u0442 \u043A\
  \u0440\u0430\u0442\u043A\u0438\u0439 \u043E\u0431\u0437\u043E\u0440."
lastmod: '2024-03-13T22:44:44.579877-06:00'
model: gpt-4-0125-preview
summary: "\u0412 TypeScript \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\
  \u0435 \u043C\u043E\u0436\u043D\u043E \u0432\u044B\u043F\u043E\u043B\u043D\u0438\
  \u0442\u044C \u043D\u0435\u0441\u043A\u043E\u043B\u044C\u043A\u0438\u043C\u0438\
  \ \u0441\u043F\u043E\u0441\u043E\u0431\u0430\u043C\u0438."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как это сделать:
В TypeScript округление можно выполнить несколькими способами. Вот краткий обзор:

```typescript
// Math.round округляет до ближайшего целого числа
console.log(Math.round(1.5)); // Вывод: 2

// Math.ceil округляет вверх до ближайшего целого числа
console.log(Math.ceil(1.1)); // Вывод: 2

// Math.floor округляет вниз до ближайшего целого числа
console.log(Math.floor(1.8)); // Вывод: 1

// toFixed округляет до фиксированного числа десятичных знаков
let num = 1.23456;
console.log(num.toFixed(2)); // Вывод: "1.23"
// Обратите внимание: toFixed возвращает строку! Используйте parseFloat для обратного преобразования, если это необходимо.
console.log(parseFloat(num.toFixed(2))); // Вывод: 1.23
```

## Углубленный разбор
В старые времена округление было необходимостью из-за ограниченного пространства и проблем с точностью на ранних компьютерах. Сегодня арифметика с плавающей точкой может привести к странным результатам из-за того, как числа хранятся в двоичной форме. Альтернативы округлению включают floor, ceil и trunc (для отсечения десятичных дробей без округления).

Внутренности заслуживают внимания: `Math.round` следует принципу "округление вверх от половины" (также известное как "коммерческое округление"), в то время как `Math.floor` и `Math.ceil` работают просто. `toFixed` может вызвать неожиданные результаты, потому что возвращает строку, и округляет, используя "округление до ближайшего четного" (также известное как "банковское округление"), что особенно полезно для уменьшения предвзятости при многократном округлении одних и тех же чисел.

## Смотрите также
- [MDN - Math.round()](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE Стандарт для арифметики с плавающей точкой (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
