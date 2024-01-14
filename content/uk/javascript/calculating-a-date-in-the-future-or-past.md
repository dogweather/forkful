---
title:                "Javascript: Обчислення дати у майбутньому або минулому"
simple_title:         "Обчислення дати у майбутньому або минулому"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому
Розрахунок дати в майбутньому або в минулому може бути корисним для створення функціональності веб-сайтів, програм або інших проєктів. Наприклад, ви можете використовувати цей метод для відображення таймера або для обчислення дати події в майбутньому або минулому.

## Як робити
Безперечно, при використанні Javascript є кілька шляхів що до обчислення дати в майбутньому або минулому, але найбільш швидкий і простий спосіб - використання вбудованих функцій Date. Використовуйте ```Javascript new Date() ``` для створення змінної, що містить поточну дату, а потім використайте методи ```Javascript setDate() ```і ```Javascript getDate() ``` для зміни дати і отримання нової дати.

```Javascript
let currentDate = new Date(); //створення змінної з поточною датою
let futureDate = currentDate.setDate(currentDate.getDate() + 7); //отримання дати через 7 днів у майбутньому
let pastDate = currentDate.setDate(currentDate.getDate() - 14); //отримання дати 14 днів тому
console.log(new Date(futureDate)); //виведення дати в новому форматі
console.log(new Date(pastDate));
```

Ви можете також використовувати функцію ```Javascript getDate() ``` для отримання конкретної дати в майбутньому або минулому за допомогою передачі аргументу, що відповідає кількості днів. Наприклад:

```Javascript
let futureDate = currentDate.getDate() + 30; //отримання дати через 30 днів у майбутньому
console.log(new Date(futureDate)); 
```

## Глибокий погляд
Якщо ви хочете отримати більш повну інформацію про обрахунок дати в майбутньому або минулому, варто дослідити інші методи Date, такі як ```Javascript getFullYear() ```, ```Javascript getMonth() ``` і ```Javascript getDay() ```. Крім того, ви можете використовувати бібліотеки, наприклад Moment.js, для спрощення процесу обчислення дат.

## Дивись також
- [MDN документація про методи Date](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js бібліотека для роботи з датами](https://momentjs.com/)