---
title:                "Преобразование строки в нижний регистр"
aliases:
- /ru/javascript/converting-a-string-to-lower-case.md
date:                  2024-01-28T23:56:49.776536-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в нижний регистр означает изменение всех символов в ней на их аналоги в нижнем регистре. Программисты делают это для обеспечения консистентности, особенно для сравнений без учета регистра, например, при нормализации пользовательского ввода или поиске в тексте.

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
