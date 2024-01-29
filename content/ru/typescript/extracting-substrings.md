---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:57:41.386553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?
Извлечение подстрок означает выделение конкретных фрагментов строки. Это удобно для задач, таких как анализ данных, проверка ввода или просто разбиение текста на более управляемые части.

## Как это делать:
В TypeScript строки разбиваются на части с помощью методов вроде `substring()`, `slice()` и `includes()` из ES6 для поиска текста в строках.

```TypeScript
let fullString: string = "Привет, энтузиасты TypeScript!";

// Взять с 7 по 18 символ
let substr: string = fullString.substring(7, 18);
console.log(substr); // Выводит: TypeScript

// То же самое, но с использованием slice()
let sliced: string = fullString.slice(7, 18);
console.log(sliced); // Выводит: TypeScript

// Проверка на существование подстроки
let exists: boolean = fullString.includes("TypeScript");
console.log(exists); // Выводит: true
```

## Глубже:
Когда-то манипуляции со строками были более неудобными — подумайте о функциях строк в C. Теперь JavaScript и TypeScript предлагают методы, которые обрабатывают Unicode, учитывают кодировку символов и работают непосредственно с объектами строк. `substring()` и `slice()` похожи, но с нюансом: `slice()` может принимать отрицательные индексы, обратно отсчитывая от конца. `substring()` обрабатывает их как нули. В ситуациях, когда важна производительность, выбор между ними может иметь значение, но для повседневного использования это примерно одно и то же.

```TypeScript
// Использование отрицательного индекса с slice
let endSliced: string = fullString.slice(-25, -7);
console.log(endSliced); // Выводит: Привет, Type
```

Что касается `includes()`, это находка для читаемости по сравнению с классическим `indexOf()`, делая ваши намерения понятными с первого взгляда. Больше нет необходимости в `if (string.indexOf('некий текст') !== -1)`; достаточно прямолинейного `if (string.includes('некий текст'))`.

## Смотрите также
- Руководство по TypeScript о строках, для более подробного ознакомления с использованием типов `'string'`: [Строка TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- Документация MDN Web Docs о методах строк в JavaScript, применимых для TypeScript: [Строка MDN](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String)
- Для понимания больше о Unicode и JavaScript (а следовательно, и TypeScript), ознакомьтесь с [Понимание внутренней кодировки символов в JavaScript: UCS-2? UTF-16?](http://mathiasbynens.be/notes/javascript-encoding)
