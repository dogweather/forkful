---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:42.231187-07:00
description: "\u042F\u043A: \u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0442\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\
  \u044F \u0430\u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445\
  \ \u043C\u0430\u0441\u0438\u0432\u0456\u0432 \u0432 TypeScript \u0454 \u043F\u0440\
  \u043E\u0441\u0442\u0438\u043C. \u041E\u0441\u044C \u043E\u0441\u043D\u043E\u0432\
  \u043D\u0456 \u043A\u0440\u043E\u043A\u0438."
lastmod: '2024-03-13T22:44:48.857093-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0430 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\u0441\u043E\
  \u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\u0438\u0432\
  \u0456\u0432 \u0432 TypeScript \u0454 \u043F\u0440\u043E\u0441\u0442\u0438\u043C\
  ."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0430\
  \u0441\u043E\u0446\u0456\u0430\u0442\u0438\u0432\u043D\u0438\u0445 \u043C\u0430\u0441\
  \u0438\u0432\u0456\u0432"
weight: 15
---

## Як:
Створення та використання асоціативних масивів в TypeScript є простим. Ось основні кроки:

```TypeScript
// Оголошення асоціативного масиву
let user: { [key: string]: string } = {};

// Додавання даних
user["name"] = "Джейн Доу";
user["email"] = "jane@example.com";

console.log(user);
```

Вивід:

```TypeScript
{ name: 'Джейн Доу', email: 'jane@example.com' }
```

Ітерування по парам ключ-значення теж легке:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Вивід:

```TypeScript
name: Джейн Доу
email: jane@example.com
```

І якщо ви маєте справу зі змішанням типів даних, система типів TypeScript стане в пригоді:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "Джон Доу";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Вивід:

```TypeScript
{ name: 'Джон Доу', age: 30 }
```

## Поглиблене вивчення
В TypeScript, те, що ми називаємо асоціативними масивами, насправді є об'єктами. Історично, в мовах як-от PHP, асоціативні масиви є фундаментальним типом, але JavaScript (і, відповідно, TypeScript) використовує об'єкти для цих цілей. Цей підхід є одночасно силою та обмеженням. Об'єкти забезпечують високодинамічну структуру для асоціювання рядків зі значеннями, але вони не призначені для використання як "масиви" у традиційному розумінні. Наприклад, ви не можете прямо застосовувати до цих об'єктів методи масиву, такі як `push` або `pop`.

Для випадків, коли вам потрібні впорядковані колекції пар ключ-значення з подібними до масиву операціями, TypeScript (і сучасний JavaScript) пропонує об'єкт `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Джейн Доу");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Хоча система типів TypeScript та особливості ES6, як-от `Map`, пропонують потужні альтернативи, розуміння способу використання об'єктів як асоціативних масивів корисне для сценаріїв, де літерали об'єктів ефективніші, або при роботі зі структурами даних JSON. Головне - правильний вибір інструменту для завдання.
