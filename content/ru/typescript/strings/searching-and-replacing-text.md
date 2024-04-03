---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:12.363137-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: TypeScript, \u043E\u0441\u043D\u043E\u0432\u044B\u0432\u0430\u044F\
  \u0441\u044C \u043D\u0430 JavaScript, \u0440\u0430\u0441\u043F\u043E\u043B\u0430\
  \u0433\u0430\u0435\u0442 \u0443\u0434\u043E\u0431\u043D\u044B\u043C\u0438 \u043C\
  \u0435\u0442\u043E\u0434\u0430\u043C\u0438 \u0434\u043B\u044F \u043C\u0430\u043D\
  \u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0438 \u0441\u043E \u0441\u0442\u0440\
  \u043E\u043A\u0430\u043C\u0438. \u041C\u044B \u043C\u043E\u0436\u0435\u043C \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:44.562045-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u043E\u0441\u043D\u043E\u0432\u044B\u0432\u0430\u044F\u0441\
  \u044C \u043D\u0430 JavaScript, \u0440\u0430\u0441\u043F\u043E\u043B\u0430\u0433\
  \u0430\u0435\u0442 \u0443\u0434\u043E\u0431\u043D\u044B\u043C\u0438 \u043C\u0435\
  \u0442\u043E\u0434\u0430\u043C\u0438 \u0434\u043B\u044F \u043C\u0430\u043D\u0438\
  \u043F\u0443\u043B\u044F\u0446\u0438\u0438 \u0441\u043E \u0441\u0442\u0440\u043E\
  \u043A\u0430\u043C\u0438."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

## Как это сделать:
TypeScript, основываясь на JavaScript, располагает удобными методами для манипуляции со строками. Мы можем использовать `String.prototype.replace()` для базовых операций поиска и замены. Ознакомьтесь с этими фрагментами кода:

```typescript
// Простая замена в строке
let text: string = "Hello, World!";
let newText: string = text.replace("World", "TypeScript");
console.log(newText);  // Вывод: Hello, TypeScript!

// Глобальная замена с regex
let regexText: string = "foo bar foo bar";
let globalRegex: RegExp = /foo/g;
let newRegexText: string = regexText.replace(globalRegex, "baz");
console.log(newRegexText);  // Вывод: baz bar baz bar

// Замена с использованием функции
let dynamicText: string = "I have 2 apples and 5 oranges.";
let fruitCounter: string = dynamicText.replace(/\d+/g, (match) => {
    return (+match * 2).toString();
});
console.log(fruitCounter);  // Вывод: I have 4 apples and 10 oranges.
```

## Глубокое погружение
Исторически, замена текста была возможностью даже в самых ранних инструментах обработки текста, с иконическими примерами вроде Unix-утилиты `sed`. В более современном программировании операции замены часто бывают более мощными при сочетании с регулярными выражениями (regex) для сопоставления с образцами.

Альтернативы `String.prototype.replace()` в TypeScript множественны. Библиотеки вроде Lodash предлагают `_.replace()` с похожим синтаксисом. Для более сложных сценариев вы можете рассмотреть возможность создания собственного парсера или использования библиотек парсеров для задач трансформации, выходящих за рамки простой замены строк.

Когда мы говорим о реализации, помните, что `.replace()` не изменяет исходную строку. Строки в JavaScript и TypeScript неизменяемы. Метод возвращает новую строку, поэтому, если вам нужен измененный текст, вам придется его сохранить, как в примерах выше.

## Смотрите также
- MDN Web Docs по `replace()`: [MDN String replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Инструмент для тестирования regex, чтобы отточить ваши навыки сопоставления с образцом: [Regex101](https://regex101.com/)
- Замена строки в Lodash для альтернативного подхода: [Lodash _.replace](https://lodash.com/docs/4.17.15#replace)
