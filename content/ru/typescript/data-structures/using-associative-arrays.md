---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:31.678947-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0438 \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u0430\u0441\
  \u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\u0445 \u043C\u0430\u0441\
  \u0441\u0438\u0432\u043E\u0432 \u0432 TypeScript \u043F\u0440\u043E\u0441\u0442\u043E\
  . \u0412\u043E\u0442 \u0431\u0430\u0437\u043E\u0432\u0430\u044F \u0438\u043D\u0441\
  \u0442\u0440\u0443\u043A\u0446\u0438\u044F."
lastmod: '2024-03-13T22:44:44.576275-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0438 \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u0430\u0441\u0441\u043E\
  \u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\u0445 \u043C\u0430\u0441\u0441\u0438\
  \u0432\u043E\u0432 \u0432 TypeScript \u043F\u0440\u043E\u0441\u0442\u043E."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0430\u0441\u0441\u043E\u0446\u0438\u0430\u0442\u0438\u0432\u043D\u044B\u0445\
  \ \u043C\u0430\u0441\u0441\u0438\u0432\u043E\u0432"
weight: 15
---

## Как использовать:
Создание и использование ассоциативных массивов в TypeScript просто. Вот базовая инструкция:

```TypeScript
// Объявление ассоциативного массива
let user: { [key: string]: string } = {};

// Добавление данных
user["name"] = "Джейн Доу";
user["email"] = "jane@example.com";

console.log(user);
```

Вывод:

```TypeScript
{ name: 'Джейн Доу', email: 'jane@example.com' }
```

Итерация по парам ключ-значение тоже проста:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

Вывод:

```TypeScript
name: Джейн Доу
email: jane@example.com
```

И если вы работаете с комбинацией типов данных, система типов TypeScript приходит на помощь:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "Джон Доу";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

Вывод:

```TypeScript
{ name: 'Джон Доу', age: 30 }
```

## Глубокое погружение
В TypeScript то, что мы называем ассоциативными массивами, по сути, являются объектами. Исторически, в языках вроде PHP, ассоциативные массивы являются основным типом, но JavaScript (и, как следствие, TypeScript) использует для этих целей объекты. Этот подход является как силой, так и ограничением. Объекты предоставляют высоко-динамичную структуру для ассоциирования строк со значениями, но они не предназначены для использования в качестве 'массивов' в традиционном смысле. Например, вы не можете напрямую использовать методы массивов вроде `push` или `pop` на этих объектах.

Для случаев, когда вам нужны упорядоченные коллекции пар ключ-значение с операциями, похожими на массивы, TypeScript (и современный JavaScript) предлагают объект `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Джейн Доу");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

Пока система типов TypeScript и возможности ES6, вроде `Map`, предоставляют мощные альтернативы, понимание того, как использовать объекты в качестве ассоциативных массивов, полезно для сценариев, где литералы объектов более эффективны или при работе со структурами данных JSON. Всё сводится к выбору правильного инструмента для работы.
