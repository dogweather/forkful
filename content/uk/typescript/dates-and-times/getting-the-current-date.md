---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:42.445757-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 TypeScript \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u043E\
  \u0431'\u0454\u043A\u0442 `Date`, \u0449\u043E\u0431 \u043E\u0442\u0440\u0438\u043C\
  \u0430\u0442\u0438 \u043F\u043E\u0442\u043E\u0447\u043D\u0443 \u0434\u0430\u0442\
  \u0443 \u0442\u0430 \u0447\u0430\u0441. \u041E\u0441\u044C \u044F\u043A \u0446\u0435\
  \ \u043C\u043E\u0436\u043D\u0430 \u0437\u0440\u043E\u0431\u0438\u0442\u0438."
lastmod: '2024-03-13T22:44:48.884284-06:00'
model: gpt-4-0125-preview
summary: "\u0423 TypeScript \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438\
  \ \u043E\u0431'\u0454\u043A\u0442 `Date`, \u0449\u043E\u0431 \u043E\u0442\u0440\u0438\
  \u043C\u0430\u0442\u0438 \u043F\u043E\u0442\u043E\u0447\u043D\u0443 \u0434\u0430\
  \u0442\u0443 \u0442\u0430 \u0447\u0430\u0441."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## Як це зробити:
У TypeScript ви можете використовувати об'єкт `Date`, щоб отримати поточну дату та час. Ось як це можна зробити:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Приклад виводу:
```
2023-04-12T07:20:50.52Z
```

Цей фрагмент коду створює новий об'єкт `Date`, який містить поточну дату та час, що потім виводиться на консоль. Ви також можете форматувати дату за допомогою toLocaleDateString() для більш зрозумілого формату:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Приклад виводу:
```
12.04.2023
```

### Використання date-fns
Для більш широких маніпуляцій з датою та форматування бібліотека `date-fns` є популярним вибором. Спочатку встановіть її через npm:

```bash
npm install date-fns
```

Потім ви можете використовувати її для форматування поточної дати:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Приклад виводу:
```
2023-04-12
```

Цей приклад з `date-fns` форматує поточну дату як рядок у форматі "РРРР-ММ-ДД". Бібліотека пропонує безліч функцій для маніпуляцій з датами, що робить її універсальним інструментом для будь-якого програміста TypeScript, який працює з датами.
