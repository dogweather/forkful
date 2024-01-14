---
title:                "TypeScript: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому
Часто, коли програмісти працюють зі стрічками в TypeScript, їм потрібно змінити регістр окремих символів у стрічці. Це може бути необхідно для того, щоб вивести стрічку в певному форматі, або для порівняння стрічок без врахування регістру символів. Також це може бути корисно при валідації введених даних.

## Як досягти

Існує кілька різних способів змінити регістр в стрічці в TypeScript, але найпростішим і найпоширенішим є використання методу `.toUpperCase()` для зміни всіх символів на великі, або `.toLowerCase()` для зміни на малі. Наприклад:

```TypeScript
let str = "ПрИкЛаД";
console.log(str.toUpperCase()); // виведе "ПРИКЛАД"
```

Іншим варіантом є використання методу `.replace()` для заміни конкретної частини стрічки на великі або малі символи. Наприклад:

```TypeScript
let str = "ПрИкЛаД";
console.log(str.replace("кЛ", "кл").toUpperCase()); // виведе "ПРИКЛАД"
```

Також існує можливість використання методу `.charAt()` для зміни регістру лише одного символу. Наприклад:

```TypeScript
let str = "прИклАд";
console.log(str.charAt(0).toUpperCase() + str.slice(1)); // виведе "ПрИклАд"
```

## Глибоке занурення

Більш складні завдання щодо зміни регістру можна досягнути шляхом використання різних регулярних виразів і методів. Наприклад, для зміни першої літери кожного слова на велику, можна застосувати метод `.replace()` з використанням регулярного виразу `/(\b\w)/g`, який вибирає першу літеру кожного слова, і методу `.toUpperCase()` для зміни регістру. Наприклад:

```TypeScript
let str = "прИклАд з ПЕРШОГО слова";
console.log(str.replace(/(\b\w)/g, (match) => match.toLocaleUpperCase())); // виведе "ПрИклАд З ПЕРШОГО Слова"
```

Цей приклад може бути складнішим для розуміння, але дозволяє змінювати регістр не тільки в одному конкретному місці, а у всій стрічці.

## Дивись також

- [Документація TypeScript](https://www.typescriptlang.org/docs/)
- [Регулярні вирази в TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Використання методів рядків в TypeScript](https://www.tutorialspoint.com/typescript/typescript_strings.htm)