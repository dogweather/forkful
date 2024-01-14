---
title:                "TypeScript: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні дуже часто виникає необхідність порівнювати дві дати. Це може бути корисно, наприклад, для визначення того, чи подія вже минула, чи ще не відбулася. В цій статті я розповім вам про те, як порівнювати дати в TypeScript та дам приклади коду з його використанням.

## Як це зробити

Для початку нам потрібно створити дві змінні з типом `Date` та присвоїти їм значення дат. Далі ми можемо використовувати різні методи для порівняння дат.

```TypeScript
let date1: Date = new Date(2021, 3, 14);
let date2: Date = new Date(2021, 3, 15);

console.log(date1 < date2); // виводиться true
console.log(date1 > date2); // виводиться false
console.log(date1 === date2); // виводиться false
```

Якщо ми хочемо порівняти дати з точністю до дня, ми можемо використовувати метод `toDateString()`, який повертає дату у вигляді строки без часу.

```TypeScript
console.log(date1.toDateString() === date2.toDateString()); // виводиться false
```

Ми також можемо використовувати методи `getTime()` та `valueOf()` для порівняння дат, оскільки вони повертають кількість мілісекунд, що пройшли з початку епохи до вказаної дати.

```TypeScript
console.log(date1.getTime() === date2.getTime()); // виводиться false
console.log(date1.valueOf() === date2.valueOf()); // виводиться false
```

## Глибоке занурення

Під час порівняння дат важливо пам'ятати про те, що об'єкти типу `Date` пов'язані зі своєю часовою зоной. Це може призвести до неточностей при порівнянні дат з різними часовими зонами. Також варто звернути увагу на те, що місяці в JavaScript/TypeScript починаються з 0, тому при створенні об'єкта типу `Date` потрібно враховувати цю особливість.

## Дивись також

- [Date](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Operator Overloading in TypeScript](https://www.typescriptlang.org/docs/handbook/2/operators.html#operator-overloading) 
- [Comparing dates in JavaScript](https://www.geeksforgeeks.org/comparing-dates-in-javascript/)