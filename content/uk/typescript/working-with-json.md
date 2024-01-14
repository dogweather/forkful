---
title:                "TypeScript: Робота з json"
simple_title:         "Робота з json"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

JSON є одним з найпопулярніших форматів обміну даними в розробці програмного забезпечення. Він простий у використанні і має широку підтримку у багатьох мовах програмування. Працюючи з JSON, ви зможете ефективно обробляти та передавати дані між різними системами.

## Як

Щоб почати роботу з JSON у TypeScript, спочатку потрібно встановити необхідну бібліотеку за допомогою менеджера пакетів, наприклад, npm. Для цього введіть наступну команду у терміналі:

```
npm install -save json
```

Після цього використовуйте ключове слово `import` для імпортування бібліотеки у вашому файлі TypeScript:

```
import * as json from 'json';
```

Тепер ви можете створювати і обробляти об'єкти JSON за допомогою методів цієї бібліотеки. Наприклад, для створення об'єкту JSON можна використовувати наступний код:

```
let person = json.parse('{"name": "John", "age": "25"}');
```
І для доступу до значень в цьому об'єкті, можна використовувати наступне:

```
console.log(person.name); // виводить "John"
console.log(person.age); // виводить "25"
```

## Глибоке занурення

У TypeScript є вбудовані типи даних для роботи з JSON - `any` та `object`. Тип `any` дозволяє приймати імпортовані дані будь-якого типу, тоді як тип `object` є більш точним і обмежує приймання даних до об'єктів. Також можна використовувати інтерфейси для зберігання структури об'єкту JSON та його валідації під час компіляції.

Додатково, у TypeScript є також декілька корисних функцій для роботи з JSON, таких як `stringify` для перетворення об'єктів у рядки JSON та `stringifyPretty` для отримання красиво відформатованого JSON.

## Дивіться також

- [Документація з JSON на сайті TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#json)
- [Стаття про роботу з JSON у TypeScript на Medium](https://medium.com/@xjamundx/custom-typescript-compiler-transforms-creating-better-apis-by-hacking-typescript-7e975712b0a9)
- [Робота з JSON у TypeScript на прикладі реального проекту](https://github.com/Ninja-Squad/ng-fixture/blob/master/projects/testing-library/src/fixtures/serialization.ts)