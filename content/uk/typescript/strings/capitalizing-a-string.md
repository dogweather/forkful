---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:10.343697-07:00
description: "\u042F\u043A: TypeScript, \u0431\u0443\u0434\u0443\u0447\u0438 \u043D\
  \u0430\u0434\u0431\u0443\u0434\u043E\u0432\u043E\u044E \u043D\u0430\u0434 JavaScript,\
  \ \u0434\u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0440\u0456\u0437\u043D\
  \u0456 \u043C\u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043F\u0435\u0440\
  \u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0437 \u043C\u0430\u043B\u043E\u0457 \u043D\u0430 \u0432\u0435\u043B\u0438\
  \u043A\u0443 \u0431\u0443\u043A\u0432\u0443, \u043F\u043E\u0447\u0438\u043D\u0430\
  \u044E\u0447\u0438 \u0432\u0456\u0434\u2026"
lastmod: '2024-03-13T22:44:48.843219-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, \u0431\u0443\u0434\u0443\u0447\u0438 \u043D\u0430\u0434\u0431\
  \u0443\u0434\u043E\u0432\u043E\u044E \u043D\u0430\u0434 JavaScript, \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0440\u0456\u0437\u043D\u0456 \u043C\
  \u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043F\u0435\u0440\u0435\u0442\
  \u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\u044F\u0434\u043A\u0456\u0432\
  \ \u0437 \u043C\u0430\u043B\u043E\u0457 \u043D\u0430 \u0432\u0435\u043B\u0438\u043A\
  \u0443 \u0431\u0443\u043A\u0432\u0443, \u043F\u043E\u0447\u0438\u043D\u0430\u044E\
  \u0447\u0438 \u0432\u0456\u0434 \u0447\u0438\u0441\u0442\u0438\u0445 \u043F\u0456\
  \u0434\u0445\u043E\u0434\u0456\u0432 JavaScript \u0442\u0430 \u0437\u0430\u043A\u0456\
  \u043D\u0447\u0443\u044E\u0447\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u0430\u043D\u043D\u044F\u043C \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A \u0434\u043B\u044F \u0431\
  \u0456\u043B\u044C\u0448 \u0441\u043A\u043B\u0430\u0434\u043D\u0438\u0445 \u0430\
  \u0431\u043E \u0441\u043F\u0435\u0446\u0438\u0444\u0456\u0447\u043D\u0438\u0445\
  \ \u0432\u0438\u043F\u0430\u0434\u043A\u0456\u0432."
title: "\u0417\u0440\u043E\u0431\u0438\u0442\u0438 \u043F\u0435\u0440\u0448\u0443\
  \ \u043B\u0456\u0442\u0435\u0440\u0443 \u0440\u044F\u0434\u043A\u0430 \u0432\u0435\
  \u043B\u0438\u043A\u043E\u044E"
weight: 2
---

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
