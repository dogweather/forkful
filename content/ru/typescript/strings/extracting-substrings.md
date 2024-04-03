---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:41.386553-07:00
description: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ \u0432\u044B\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u043A\u043E\u043D\u043A\
  \u0440\u0435\u0442\u043D\u044B\u0445 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\
  \u043E\u0432 \u0441\u0442\u0440\u043E\u043A\u0438. \u042D\u0442\u043E \u0443\u0434\
  \u043E\u0431\u043D\u043E \u0434\u043B\u044F \u0437\u0430\u0434\u0430\u0447, \u0442\
  \u0430\u043A\u0438\u0445 \u043A\u0430\u043A \u0430\u043D\u0430\u043B\u0438\u0437\
  \ \u0434\u0430\u043D\u043D\u044B\u0445, \u043F\u0440\u043E\u0432\u0435\u0440\u043A\
  \u0430 \u0432\u0432\u043E\u0434\u0430 \u0438\u043B\u0438 \u043F\u0440\u043E\u0441\
  \u0442\u043E \u0440\u0430\u0437\u0431\u0438\u0435\u043D\u0438\u0435\u2026"
lastmod: '2024-03-13T22:44:44.569195-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\
  \u0434\u0441\u0442\u0440\u043E\u043A \u043E\u0437\u043D\u0430\u0447\u0430\u0435\u0442\
  \ \u0432\u044B\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u043A\u043E\u043D\u043A\
  \u0440\u0435\u0442\u043D\u044B\u0445 \u0444\u0440\u0430\u0433\u043C\u0435\u043D\u0442\
  \u043E\u0432 \u0441\u0442\u0440\u043E\u043A\u0438."
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
weight: 6
---

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
