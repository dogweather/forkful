---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:49.776536-07:00
description: "\u041A\u0430\u043A: \u0412 JavaScript \u043C\u044B \u043F\u0440\u0435\
  \u043E\u0431\u0440\u0430\u0437\u0443\u0435\u043C \u0441\u0442\u0440\u043E\u043A\u0443\
  \ \u0432 \u043D\u0438\u0436\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\
  \u0440 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u043C\u0435\u0442\u043E\
  \u0434\u0430 `.toLowerCase()`. \u042D\u0442\u043E \u043F\u0440\u043E\u0441\u0442\
  \u043E \u043A\u0430\u043A."
lastmod: '2024-03-13T22:44:45.736526-06:00'
model: gpt-4-0125-preview
summary: "\u0412 JavaScript \u043C\u044B \u043F\u0440\u0435\u043E\u0431\u0440\u0430\
  \u0437\u0443\u0435\u043C \u0441\u0442\u0440\u043E\u043A\u0443 \u0432 \u043D\u0438\
  \u0436\u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440 \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u043C\u0435\u0442\u043E\u0434\u0430 `.toLowerCase()`."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как:
В JavaScript мы преобразуем строку в нижний регистр с помощью метода `.toLowerCase()`. Это просто как:

```javascript
let greeting = "Привет, Мир!";
let lowerCaseGreeting = greeting.toLowerCase();
console.log(lowerCaseGreeting); // "привет, мир!"
```

При использовании каждый символ в исходной строке, если возможно, преобразуется в нижний регистр:

```javascript
let mixedCase = "jAvAScript ROCKs!";
let lowerCased = mixedCase.toLowerCase();
console.log(lowerCased); // "javascript rocks!"
```

Обратите внимание, что символы, не имеющие эквивалента в нижнем регистре, остаются без изменений.

## Погружение
В стародавние времена обработка текста означала необходимость быть осторожным с кодировками символов и ручными преобразованиями. Но в современном JavaScript метод `.toLowerCase()` скрывает эти сложности. Внутри он использует сопоставления Unicode для преобразования символов, так что это работает не только с A-Z.

Существуют и альтернативные методы, например:

- `toLocaleLowerCase()`: Учитывает локаль пользователя, что делает его необходимым для определенных языков, где правила перевода в нижний регистр зависят от контекста.

- Регулярные выражения: До `toLowerCase()` разработчики могли использовать регекс для ручной замены символов верхнего регистра.

Подробности: помните, что `.toLowerCase()` не изменяет исходную строку (строки в JavaScript неизменяемы). Вы всегда получаете новую строку. Он также обрабатывает все символы, распознаваемые стандартом Unicode как символы верхнего регистра, что означает, что вы защищены на разных языках и скриптах.

## Смотрите также
- [MDN web docs по toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Стандарт Unicode для регистров](https://unicode.org/reports/tr21/tr21-5.html)
- [Верхний и нижний регистры с учетом локали: toLocaleLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
