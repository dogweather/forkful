---
title:                "Поиск и замена текста"
date:                  2024-01-29T00:02:00.380736-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Поиск и замена текста означают нахождение определённых подстрок и их замену на что-то новое. Зачем это нужно? Это повсюду: исправление опечаток в документе, рефакторинг кода, или пакетное редактирование данных.

## Как это сделать:
В JavaScript `String.prototype.replace()` является основным инструментом. Передайте строку или регулярное выражение и замену. Вот краткий пример:

```javascript
let str = "I love to code in JavaScript!";
let newStr = str.replace("JavaScript", "TypeScript");
console.log(newStr); // Выводит: I love to code in TypeScript!
```

Теперь с использованием регулярных выражений для глобальных замен:

```javascript
let story = "The quick brown fox jumps over the lazy dog. The fox is clever.";
let newStory = story.replace(/fox/g, "cat");
console.log(newStory); // Выводит: The quick brown cat jumps over the lazy dog. The cat is clever.
```

## Глубокое погружение
Оглядываясь назад, `String.prototype.replace()` был в JS с самого начала — времён Netscape 2. Теперь же, ES6 принёс нам строковые шаблоны и стрелочные функции, которые добавили возможностей для более краткого и читаемого кода с использованием регулярных выражений.

Альтернативы? Конечно. Если вы работаете с обработкой больших объёмов текста, возможно, вы перейдёте к потокам Node.js или воспользуетесь внешними библиотеками для обработки сложных паттернов, эффективности и производительности.

Что касается реализации, `replace()` сам по себе прост. Но паттерны регулярных выражений могут быть запутанными. Начните с простого, изучите специальные символы (`.` соответствует любому символу, `*` для повторяющихся паттернов) и тестируйте с инструментами вроде regex101.

## Смотрите также
- Документация MDN по replace: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 для тестирования выражений: https://regex101.com/
- Информация о регулярных выражениях в JavaScript: https://javascript.info/regexp-introduction
