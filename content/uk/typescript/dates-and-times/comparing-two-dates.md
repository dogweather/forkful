---
date: 2024-01-20 17:34:13.096247-07:00
description: "Por\u0456vnyannya dvokh dat - tse sposib vstanovlennya chasovoyi r\u0456\
  znits\u0456 m\u0456zh nimi. Program\u0456sti robyat\u02B9 tse, shchob v\u0456dkr\u0456\
  plyuvaty podiyi v chas\u0456, zapuskaty\u2026"
lastmod: '2024-03-13T22:44:48.887133-06:00'
model: gpt-4-1106-preview
summary: "Por\u0456vnyannya dvokh dat - tse sposib vstanovlennya chasovoyi r\u0456\
  znits\u0456 m\u0456zh nimi. Program\u0456sti robyat\u02B9 tse, shchob v\u0456dkr\u0456\
  plyuvaty podiyi v chas\u0456, zapuskaty\u2026"
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Що і Чому?
Porіvnyannya dvokh dat - tse sposib vstanovlennya chasovoyi rіznitsі mіzh nimi. Programіsti robyatʹ tse, shchob vіdkrіplyuvaty podiyi v chasі, zapuskaty terminovі zadachi, abo perevіryaty chinnіstʹ danih.

## Як зробити:
```TypeScript
// Створюємо дві дати
const date1 = new Date('2023-04-01T00:00:00');
const date2 = new Date('2023-04-02T00:00:00');

// Порівнюємо дати
if (date1 < date2) {
  console.log('date1 є раніше date2');
} else if (date1 > date2) {
  console.log('date1 є пізніше date2');
} else {
  console.log('date1 та date2 є однакові');
}

// Вивід: 'date1 є раніше date2'
```

## Поглиблений огляд
В JavaScript і TypeScript дати порівнюються як числа, так як вони представлені кількістю мілісекунд з 1970 року (Unix Time Stamp). Часові зони можуть впливати на результати порівняння, тому розробники мають бути уважні при роботі з ними. Альтернативами є бібліотеки, наприклад Moment.js або date-fns, які можуть спрощувати роботу з датами і часовими зонами.

## Дивіться також:
- MDN Web Docs про роботу з датами у JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Date-fns бібліотека для роботи з датами: https://date-fns.org/
- Moment.js бібліотека та документація: https://momentjs.com/docs/#/parsing/unix-timestamp/
