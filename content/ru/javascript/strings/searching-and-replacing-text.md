---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:00.380736-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 JavaScript `String.prototype.replace()` \u044F\u0432\u043B\u044F\
  \u0435\u0442\u0441\u044F \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u043C \u0438\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C. \u041F\u0435\
  \u0440\u0435\u0434\u0430\u0439\u0442\u0435 \u0441\u0442\u0440\u043E\u043A\u0443\
  \ \u0438\u043B\u0438 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u043E\u0435\
  \ \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\u0435 \u0438 \u0437\u0430\u043C\
  \u0435\u043D\u0443. \u0412\u043E\u0442 \u043A\u0440\u0430\u0442\u043A\u0438\u0439\
  \u2026"
lastmod: '2024-03-13T22:44:45.732671-06:00'
model: gpt-4-0125-preview
summary: "\u0412 JavaScript `String.prototype.replace()` \u044F\u0432\u043B\u044F\u0435\
  \u0442\u0441\u044F \u043E\u0441\u043D\u043E\u0432\u043D\u044B\u043C \u0438\u043D\
  \u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u043E\u043C."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

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
