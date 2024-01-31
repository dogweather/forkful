---
title:                "Поиск и замена текста"
date:                  2024-01-29T00:02:12.363137-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Поиск и замена текста в строках — это распространенная задача в программировании, часто используемая для обработки и манипулирования данными. Это критически важно для уточнения содержания, исправления ошибок и автоматизации правок в больших кодовых базах или наборах данных.

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
