---
title:                "Зробити першу літеру рядка великою"
date:                  2024-02-03T19:07:10.343697-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Перетворення рядка в рядок з великою першою літерою включає зміну першого символу даного рядка на велику букву, якщо він у нижньому регістрі, часто залишаючи решту рядка без змін. Ця операція зазвичай використовується для того, щоб власні назви або початки речень дотримувалися граматичних правил при обробці тексту, роблячи результати професійно виглядаючими та зручними для читання.

## Як:

TypeScript, будучи надбудовою над JavaScript, дозволяє використовувати різні методи для перетворення рядків з малої на велику букву, починаючи від чистих підходів JavaScript та закінчуючи використанням сторонніх бібліотек для більш складних або специфічних випадків.

**Чистий JavaScript підхід:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// Приклад виводу:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

Цей метод є простим і ґрунтується на методі `charAt()`, щоб отримати доступ до першого символу рядка, та `toUpperCase()`, щоб перетворити його на велику літеру. Метод `slice(1)` потім отримує решту рядка, залишаючи її без змін.

**Використання бібліотеки Lodash:**

Для проєктів, що вже використовують бібліотеку [Lodash](https://lodash.com/), ви можете скористатися її функцією `_.capitalize`, щоб досягти того ж результату з меншою кількістю шаблонного коду.

Спочатку встановіть Lodash:

```bash
npm install lodash
```

Потім використовуйте його у вашому файлі TypeScript:

```typescript
import * as _ from 'lodash';

// Приклад виводу:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

Зауважте: метод `_.capitalize` в Lodash перетворює решту рядка на нижній регістр, що може бути не завжди бажаним.

**Використання Регулярного Виразу:**

Регулярний вираз може забезпечити лаконічний спосіб перетворити першу літеру рядка у велику, особливо якщо вам треба з великої літери написати першу літеру кожного слова у рядку.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// Приклад виводу:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

Цей метод використовує функцію `replace()`, щоб шукати будь-який кордон слова, за яким слідує буквено-чисельний символ (`\b\w`), перетворюючи кожний знахідку на велику літеру. Це особливо зручно для заголовків або назв.