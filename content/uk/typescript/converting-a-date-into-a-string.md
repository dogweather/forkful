---
title:    "TypeScript: Перетворення дати у рядок."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Перетворення дати в рядок є важливим завданням, яке зустрічається у багатьох програмах. Це допоможе користувачу отримати зрозумілу та зручну форму дати, яку можна вивести на екран або зберегти у файл. Крім того, конвертація дати в рядок дозволяє здійснити різноманітні обчислення, перевірки та порівняння.

## Як

```TypeScript
let date = new Date(); 
let dateAsString = date.toDateString(); 
console.log(dateAsString);
```

У цьому прикладі ми створюємо об'єкт дати за допомогою конструктора `new Date()` і зберігаємо його у змінній `date`. Потім ми використовуємо метод `toDateString()`, який перетворює дату в рядок у форматі "день тижня, місяць день, рік". Наприклад, результатом виконання коду буде `Tue May 18 2021`. Цей рядок ми зберігаємо у змінній `dateAsString` та виводимо на екран за допомогою `console.log()`.

## Deep Dive

Конвертація дати в рядок може бути корисною не лише для виведення дати на екран, але й для подальшої обробки та форматування. Наприклад, за допомогою різних методів, які мають об'єкт `Date`, можна отримати окремі значення дати, такі як день, місяць або рік. Також існує можливість встановлення будь-якого формату дати за допомогою методу `toLocaleDateString()`. Детальніше про методи та формати можна дізнатися з [офіційної документації TypeScript](https://www.typescriptlang.org/docs/handbook/standard-library.html#date).

## Дивись також

- [Офіційна документація TypeScript: Робота з датами](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [Основи роботи з датами в TypeScript](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-in-typescript)
- [Порівняння дат у TypeScript](https://javascript.plainenglish.io/comparing-dates-in-typescript-ec062f51b9d2)