---
title:                "Перетворення дати у рядок"
html_title:           "TypeScript: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Що і Чому?
Конвертування дати в рядок - це процес перетворення дати в текстовий формат. Це часто використовується програмістами для збереження інформації про дату в зручному для читання форматі.

Як?
Ось декілька прикладів коду на TypeScript для конвертування дати в рядок та виводу результату:

```TypeScript
// Дзеркальне відображення дати у форматі року-місяця-дня
const date = new Date();
const stringDate = date.toLocaleDateString();
console.log(stringDate); // Output: 2021-09-01

// Довільне відображення дати у форматі дня/місяця/року
const date = new Date();
const day = String(date.getDate()).padStart(2, '0');
const month = String(date.getMonth() + 1).padStart(2, '0');
const year = date.getFullYear();
const stringDate = `${day}/${month}/${year}`;
console.log(stringDate); // Output: 01/09/2021

// Відображення дати з годиною та хвилиною у форматі день/місяць/рік година:хвилина
const date = new Date();
const year = date.getFullYear();
const month = String(date.getMonth() + 1).padStart(2, '0');
const day = String(date.getDate()).padStart(2, '0');
const hours = String(date.getHours()).padStart(2, '0');
const minutes = String(date.getMinutes()).padStart(2, '0');
const stringDate = `${day}/${month}/${year} ${hours}:${minutes}`;
console.log(stringDate); // Output: 01/09/2021 01:20
```

Глибокий занурення:
Конвертування дати в рядок використовується для забезпечення зручного збереження та відображення дати в програмах. Історично, першими методами конвертування були використання числових форматів та вручну складені шаблони для відображення дати. Проте, з появою сучасних мов програмування, таких як TypeScript, стало можливим використовувати вбудовані функції для зручного конвертування дати в рядок.

Додатково:
Ви можете дізнатися більше про конвертування дати в рядок в офіційній документації TypeScript (https://www.typescriptlang.org/docs/handbook/datetime.html) або використовувати інші альтернативні бібліотеки, такі як Moment.js, для більш розширених можливостей конвертування дати.