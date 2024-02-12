---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:57:50.211976-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Извлечение подстрок означает получение части строки. Программисты режут и нарезают строки, чтобы изолировать данные, ввести некоторый формат или изменить текст для вывода.

## Как это сделать:

### Используя метод `substring`:
```javascript
let text = "JavaScript is awesome!";
let extracted = text.substring(0, 10);
console.log(extracted); // Вывод: JavaScript
```

### Используя метод `slice`:
```javascript
let text = "JavaScript is awesome!";
let sliced = text.slice(-9, -1);
console.log(sliced); // Вывод: awesome
```

### Используя метод `substr` (устаревший):
```javascript
let text = "JavaScript is awesome!";
let substrd = text.substr(11, 7);
console.log(substrd); // Вывод: awesome
```

## Углублённо
Извлечение подстрок – не новинка, это старо как само программирование. Методы `substring` и `slice` в JavaScript – это инструменты из 1990-х, часть первоначального набора возможностей языка. `substr` тоже был в их числе, но теперь является устаревшим кодом и должен избегаться в современных приложениях.

В чем разница? `substring` и `slice` похожи – оба принимают параметры начального и конечного индекса – но по-разному обрабатывают отрицательные значения: `slice` может обрабатывать отрицательные индексы, отсчитывая их с конца, в то время как `substring` обрабатывает их как нули. Все эти методы не изменяют исходную строку; они производят новые.

## Смотрите также
- Сеть разработчиков Mozilla по строкам: [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Манипулирование строками с помощью JavaScript: [W3Schools - JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- Основы строки в JavaScript: [JavaScript.info - Strings](https://javascript.info/string)
