---
title:                "Порівняння двох дат"
html_title:           "TypeScript: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Порівняння двох дат є важливою задачею для багатьох програмістів, особливо при створенні додатків, які використовують дати. Це дозволяє перевіряти часові межі, порівнювати події та здійснювати різні обчислення з датами.

## Як це зробити

```TypeScript
// Приклад 1: Порівняння двох дат за допомогою операторів порівняння
let dateOne: Date = new Date('2020-10-10');
let dateTwo: Date = new Date('2019-10-15');

if (dateOne > dateTwo) {
    console.log('Перша дата більша за другу дату.');
} else if (dateOne < dateTwo) {
    console.log('Перша дата менша за другу дату.');
} else {
    console.log('Обидві дати рівні.');
}

// Приклад 2: Порівняння двох дат за допомогою методу "getTime()"
let dateOne: Date = new Date('2020-10-10');
let dateTwo: Date = new Date('2019-10-15');

if (dateOne.getTime() > dateTwo.getTime()) {
    console.log('Перша дата більша за другу дату.');
} else if (dateOne.getTime() < dateTwo.getTime()) {
    console.log('Перша дата менша за другу дату.');
} else {
    console.log('Обидві дати рівні.');
}

// Вихід:
// Перша дата більша за другу дату.
// Перша дата більша за другу дату.
```

## Поглиблення

JavaScript та TypeScript використовують тип даних "Date" для представлення дати та часу. Коли ми порівнюємо дві дати, ми фактично порівнюємо значення "getTime()" цих дат. Це  число представляє кількість мілісекунд, які пройшли з 1 січня 1970 року до заданої дати.

## Подивіться також

- [Документація TypeScript для типу "Date"](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)
- [Порівняння дат за допомогою вбудованих методів JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date#Comparison_functions)