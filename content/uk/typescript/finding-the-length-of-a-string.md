---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Визначення довжини рядка є операцією, яка відображає кількість символів у текстовому рядку. Програмісти роблять це, щоб контролювати і маніпулювати текстовими даними, наврізне обмежуючи або розбиваючи їх на менші частини.

## Як це зробити:
Отримання довжини рядка в TypeScript виглядає як отримання властивості `length` рядка. Подивимося на приклад.

```typescript
let name: string = "Олександр";
console.log(name.length); 
```

Ви отримаєте вихідні дані: `9`. Це дає нам довжину рядка "Олександр", яка становить 9 символів.

## Поглиблений аналіз:
1. Історичний контекст: метод `length` був наявний з самого початку JavaScript, на якому базується TypeScript. Він виявився таким же корисним і надійним для роботи з рядками у TypeScript.
2. Альтернативи: Немає прямих альтернатив методу `length` для визначення довжини рядка в TypeScript. Однак можна створити користувацькі функції для цього, хоча це зазвичай менш ефективно.
3. Деталі імплементації: Властивість `length` повертає кількість символів Unicode в рядку, що може включати символи з двох одиниць коду (наприклад, емодзі).

## Дивіться також:
1. [Типи даних в TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html)
2. [Робота з рядками в JavaScript](https://developer.mozilla.org/uk/docs/Web/JavaScript/Guide/Regular_Expressions)
3. [JavaScript рядки і їх методи](https://learn.javascript.ru/string)