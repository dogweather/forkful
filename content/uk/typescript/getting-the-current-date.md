---
title:                "Отримання поточної дати"
date:                  2024-02-03T19:11:42.445757-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати в TypeScript, мові, побудованій на основі JavaScript, дозволяє вам доступатися до поточної інформації про дату та час та маніпулювати нею. Програмісти часто потребують цю функціональність для створення часових відміток, планування та інших часово-чутливих функцій у своїх застосунках.

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
