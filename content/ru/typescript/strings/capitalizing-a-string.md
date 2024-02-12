---
title:                "Преобразование строки в верхний регистр"
aliases:
- /ru/typescript/capitalizing-a-string.md
date:                  2024-01-28T23:55:48.049592-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Преобразование строки в заглавные буквы означает превращение первой буквы каждого слова в верхний регистр, а остальные буквы - в нижний. Программисты делают это для обеспечения единообразия форматирования в пользовательских интерфейсах и для того, чтобы собственные имена и заголовки отображались корректно.

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
