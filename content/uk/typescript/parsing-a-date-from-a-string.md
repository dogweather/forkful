---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Парсинг дати з рядка - це процес, за допомогою якого ми перетворюємо текстове представлення дати в більш управліну форму, як-от об'єкт дати. Програмісти роблять це, щоб легше оброблювати та маніпулювати датами в коді. 

## Як це робиться:

Ось простий приклад того, як можна парсити дату з рядка в TypeScript:

```TypeScript
let dateStr: string = "2020-05-14"; 
let dateObj: Date = new Date(dateStr);
console.log(dateObj); 
```

На виході ви отримаєте:

```TypeScript
// Output: 2020-05-13T22:00:00.000Z 
```

Об'єкт Date створюється з певною датою, вказаною в рядку.

## Поглиблено

Iсторично JavaScript та TypeScript використовували рядки для передачі дат. Однак, цей підхід має певні цікаві аспекти. Наприклад, формат дати, використовуваний у рядку, може вплинути на результат парсинга. 

Є інші способи парсинга дати, включаючи використання бібліотек третіх сторін, як-от moment.js, які надають більш гнучкі та потужні інструменти для роботи з датами.

За реалізацією об'єкта Date в TypeScript kриється вбудований об'єкт Date в JavaScript. Тому поведінка цього об'єкта і інтерпретація рядків дати залежить від стандарту JavaScript.

## Дивіться також 

1. [Офіційна документація TypeScript по об'єкту Date](https://www.typescriptlang.org/docs/handbook/utility-types.html#datetype)
2. [Бібліотека moment.js для роботи з датами](https://momentjs.com/)