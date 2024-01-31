---
title:                "Преобразование строки в верхний регистр"
date:                  2024-01-28T23:55:33.711668-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
