---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:48.049592-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412\u043E\u0442 \u043D\u0435\u0431\u043E\u043B\u044C\u0448\u043E\
  \u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u043D\u0430 TypeScript, \u043A\u043E\
  \u0442\u043E\u0440\u044B\u0439 \u043F\u043E\u043C\u043E\u0436\u0435\u0442 \u0432\
  \u0430\u043C \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u044B\u0432\
  \u0430\u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0438."
lastmod: '2024-03-13T22:44:44.558346-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u043E\u0442 \u043D\u0435\u0431\u043E\u043B\u044C\u0448\u043E\u0439\
  \ \u043F\u0440\u0438\u043C\u0435\u0440 \u043D\u0430 TypeScript, \u043A\u043E\u0442\
  \u043E\u0440\u044B\u0439 \u043F\u043E\u043C\u043E\u0436\u0435\u0442 \u0432\u0430\
  \u043C \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u044B\u0432\u0430\
  \u0442\u044C \u0441\u0442\u0440\u043E\u043A\u0438."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

## Как это сделать:
Вот небольшой пример на TypeScript, который поможет вам преобразовывать строки:

```typescript
function capitalizeString(input: string): string {
  return input.replace(/\w\S*/g, (word) => {
    return word.charAt(0).toUpperCase() + word.substr(1).toLowerCase();
  });
}

// Пример использования:
const title = "hello world from TypeScript";
const capitalizedTitle = capitalizeString(title);
console.log(capitalizedTitle); // Вывод: "Hello World From Typescript"
```

Просто, правда? Теперь превратите эти строчные строки во что-то изысканное!

## Подробнее
Преобразование в заглавные буквы существует с времён древних письменностей, улучшая читабельность. В программировании, помимо эстетической и грамматической корректности, преобразование строк в заглавные буквы может быть критичным для операций сравнения, где "Apple" и "apple" могут рассматриваться по-разному.

Альтернативы функции `capitalizeString` могут включать в себя библиотеки, такие как Lodash, предлагающие метод `_.startCase`, или использование CSS для визуального преобразования (`text-transform: capitalize;`). Однако, CSS не изменяет фактическое значение строки, только её отображение.

В JavaScript изначально не было встроенного метода для преобразования строк в заглавные буквы, что оставляло это на усмотрение разработчиков. Функция, представленная выше, использует регулярное выражение для определения границ слова `\w\S*`, преобразует первую букву с помощью `toUpperCase()`, а остальные – с использованием `toLowerCase()`.

## Смотрите также
- Документация по строкам MDN: [https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String)
- Функция `_.startCase` от Lodash: [https://lodash.com/docs/#startCase](https://lodash.com/docs/#startCase)
- String.prototype.toLocaleUpperCase (для учёта региональных особенностей при преобразовании): [https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)
