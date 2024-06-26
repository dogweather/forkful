---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:34.548944-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C regex \u0432 JavaScript."
lastmod: '2024-03-13T22:44:45.742016-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u044C regex \u0432 JavaScript."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как это сделать:
Вот как использовать regex в JavaScript:

```javascript
// Поиск совпадения
const text = "Найти иголку в этой стоге сена";
const regex = /иголку/;
console.log(text.match(regex));
// Вывод: ["иголку"]

// Замена строки
const replacedText = text.replace(regex, "банан");
console.log(replacedText);
// Вывод: "Найти банан в этой стоге сена"

// Проверка на совпадение
const exists = regex.test(text);
console.log(exists);
// Вывод: true

// Использование флагов - 'i' для поиска без учета регистра
const caseInsensitiveRegex = /ИГОЛКУ/i;
console.log(caseInsensitiveRegex.test(text));
// Вывод: true

// Использование групп для извлечения данных
const data = "Джон: 1234, Джейн: 5678";
const groupRegex = /(\w+): (\d+)/g;
let match;
while ((match = groupRegex.exec(data)) !== null) {
  console.log(`Номер ${match[1]} это ${match[2]}`);
}
// Вывод: "Номер Джон это 1234"
// Вывод: "Номер Джейн это 5678"
```

## Погружение вглубь
Регулярные выражения используются с 1950-х годов и являются частью большинства языков программирования. Несмотря на то что они мощный инструмент для разбора текста, регулярные выражения могут быть коварны; новички часто находят их загадочными. Для более простых задач методы вроде `String.includes()`, `String.startsWith()`, и `String.endsWith()` могут служить альтернативой. Когда важна производительность, помните, что regex может быть медленным—используйте их с умом и рассмотрите оптимизацию с помощью литеральных строк или циклов для поиска отдельных символов.

## Смотрите также
- [MDN RegExp](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/RegExp) – Глубокий ресурс по regex JavaScript.
- [RegExr](https://regexr.com/) - Инструмент для изучения, построения и тестирования regex.
- [RegexOne](https://regexone.com/) - Интерактивные учебники по regex для новичков.
