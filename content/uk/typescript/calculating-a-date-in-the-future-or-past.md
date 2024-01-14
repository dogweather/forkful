---
title:                "TypeScript: Розрахунок дати в майбутньому або минулому"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Обчислення дати в майбутньому або минулому є важливою задачею для багатьох програм. Це може бути корисно для створення звітів, відображення подій або просто для обчислення терміну завершення задачі. З нами Ви дізнаєтеся, як легко і швидко це можна зробити з допомогою TypeScript.

## Як

Для початку, нам потрібно встановити бібліотеку moment.js, щоб мати можливість маніпулювати датами. Для цього використовуйте наступну команду в терміналі:

```TypeScript
npm install moment
```

Після цього, імпортуємо бібліотеку в наш файл з TypeScript кодом:

```TypeScript
import * as moment from 'moment';
```

Тепер ми можемо використовувати всі методи бібліотеки moment для роботи з датами. Для обчислення дати в майбутньому або минулому, ми можемо використати методи `add()` і `subtract()`. Наприклад, якщо нам потрібно обчислити дату, яка буде через 2 тижні, ми можемо використати наступний код:

```TypeScript
let futureDate = moment().add(2, 'weeks');
```

Щоб отримати дату, яка була 2 тижні тому, використовуйте метод `subtract()`:

```TypeScript
let pastDate = moment().subtract(2, 'weeks');
```

Для отримання форматованої дати, використовуйте метод `format()`. Наприклад, ми можемо сформатувати нашу дату в форматі "DD-MM-YYYY":

```TypeScript
let formattedDate = moment().format('DD-MM-YYYY');
```

Повний код може виглядати так:

```TypeScript
import * as moment from 'moment';

let futureDate = moment().add(2, 'weeks');
let pastDate = moment().subtract(2, 'weeks');
let formattedDate = moment().format('DD-MM-YYYY');

console.log(futureDate); // виводить дату через 2 тижні
console.log(pastDate); // виводить дату 2 тижні тому
console.log(formattedDate); // виводить дату в форматі "DD-MM-YYYY"
```

## Глибше

Бібліотека moment досить потужна і має багато інших методів для маніпулювання датами. Можна використовувати різні параметри, такі як `years`, `months`, `days` та інші, для обчислення дати в майбутньому або минулому. Крім того, у бібліотеці є функціонал для обчислення різниці між двома датами, перевірки на належність до певного періоду чи переведення дати в різні формати.

Для детального ознайомлення з функціями бібліотеки moment, рекомендуємо прочитати офіційну документац