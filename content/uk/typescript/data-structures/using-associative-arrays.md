---
title:                "Використання асоціативних масивів"
aliases:
- /uk/typescript/using-associative-arrays.md
date:                  2024-01-30T19:13:42.231187-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання асоціативних масивів"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Асоціативні масиви, або об'єкти в TypeScript, дозволяють використовувати рядки (або ключі) для доступу до пари значень. Програмісти використовують їх для більш динамічних патернів доступу до даних порівняно з традиційними масивами, забезпечуючи гнучкий спосіб структурування та доступу до даних, не будучи прив'язаними до числових індексів.

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
