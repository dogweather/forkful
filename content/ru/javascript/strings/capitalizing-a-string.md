---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:33.711668-07:00
description: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\
  \u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440 \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0435\u0442 \u0438\u0437\u043C\u0435\u043D\u0435\u043D\u0438\u0435\
  \ \u043F\u0435\u0440\u0432\u043E\u0433\u043E \u0441\u0438\u043C\u0432\u043E\u043B\
  \u0430 \u0441\u043B\u043E\u0432\u0430 \u043D\u0430 \u0431\u0443\u043A\u0432\u0443\
  \ \u0432 \u0432\u0435\u0440\u0445\u043D\u0435\u043C \u0440\u0435\u0433\u0438\u0441\
  \u0442\u0440\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E, \u0447\u0442\
  \u043E\u0431\u044B \u0441\u043B\u0435\u0434\u043E\u0432\u0430\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:45.728935-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\
  \u043D\u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440 \u043E\u0437\u043D\
  \u0430\u0447\u0430\u0435\u0442 \u0438\u0437\u043C\u0435\u043D\u0435\u043D\u0438\u0435\
  \ \u043F\u0435\u0440\u0432\u043E\u0433\u043E \u0441\u0438\u043C\u0432\u043E\u043B\
  \u0430 \u0441\u043B\u043E\u0432\u0430 \u043D\u0430 \u0431\u0443\u043A\u0432\u0443\
  \ \u0432 \u0432\u0435\u0440\u0445\u043D\u0435\u043C \u0440\u0435\u0433\u0438\u0441\
  \u0442\u0440\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442 \u044D\u0442\u043E, \u0447\u0442\
  \u043E\u0431\u044B \u0441\u043B\u0435\u0434\u043E\u0432\u0430\u0442\u044C\u2026"
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
---

{{< edit_this_page >}}

## Что и почему?
Преобразование строки в верхний регистр означает изменение первого символа слова на букву в верхнем регистре. Программисты делают это, чтобы следовать языковым конвенциям, улучшить читаемость или форматировать текст, например заголовки.

## Как это сделать:
В JavaScript нет встроенного метода для преобразования в верхний регистр, но вот простая функция, которая решает эту задачу:

```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('hello')); // Вывод: Hello
```

Для нескольких слов:

```javascript
function capitalizeWords(str) {
    return str.split(' ').map(capitalizeFirstLetter).join(' ');
}

console.log(capitalizeWords('hello world!')); // Вывод: Hello World!
```

## Подробнее
Преобразование строк в верхний регистр не всегда имело встроенные функции в языках, часто влекло за собой ручное управление ASCII. Сегодня большинство языков программирования предлагают методы для манипуляции со строками, но в JavaScript требуется более самостоятельный подход.

### Альтернативы:
Вы могли бы использовать CSS для преобразования текста в верхний регистр на веб-страницах (`text-transform: capitalize;`), или библиотеки, как Lodash, имеют функции преобразования. Но делать это с использованием чистого JavaScript, как показано выше, не требует зависимостей.

### Детали реализации:
`charAt(0)` захватывает первый символ. `toUpperCase()` преобразует его в верхний регистр. Сочетание его с оставшейся частью строки `slice(1)` дает вам строку с заглавной буквы. Этот метод хорошо работает, предполагая, что входные данные являются строкой и не начинаются с пробела.

## Смотрите также:
- CSS-трансформация текста MDN для преобразования: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform
- Документация метода преобразования Lodash: https://lodash.com/docs/4.17.15#capitalize
- JavaScript String.prototype.toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
