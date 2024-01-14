---
title:    "TypeScript: Отримання поточної дати"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Чому
Не існують програми без дати. Отримання поточної дати є важливим аспектом програмування, оскільки вона дозволяє визначати час та порядок подій у програмі.

## Як це зробити
За допомогою TypeScript можна легко отримати поточну дату та час. Нижче наведені два способи цього зробити.

```TypeScript
// Використовуємо конструктор Date()
const now = new Date();
console.log(now);

// Використовуємо метод Date.now()
const currentTime = Date.now();
console.log(currentTime);
```

У першому прикладі, ми використали конструктор `Date()` для отримання об'єкту типу `Date`, який містить поточну дату та час.

У другому прикладі, використовується метод `Date.now()` для повернення поточного часу в мілісекундах від 1 січня 1970 року. Цей метод дуже корисний, оскільки повертає значення типу `number`, яке легко обробляти та зберігати.

Обидва ці способи повертають поточну дату та час, але ми також можемо отримати окремо день, місяць, рік, годину, хвилину та секунду за допомогою відповідних методів `getDate()`, `getMonth()`, `getFullYear()`, `getHours()`, `getMinutes()`, `getSeconds()`, як показано нижче:

```TypeScript
console.log(now.getDate()); // повертає день місяця (число)
console.log(now.getMonth()); // повертає місяць (від 0 до 11)
console.log(now.getFullYear()); // повертає рік (наприклад, 2021)
console.log(now.getHours()); // повертає годину (від 0 до 23)
console.log(now.getMinutes()); // повертає хвилину (від 0 до 59)
console.log(now.getSeconds()); // повертає секунду (від 0 до 59)
```

## Глибинний аналіз
Отримання поточної дати в TypeScript базується на конструкторі `Date()` та його методах. Але важливо знати, що ці методи враховують часовий пояс устройства, на якому виконується програма. Це означає, що можуть виникнути порушення, якщо програма виконується на устройстві з іншим часовим поясом.

Також варто знати, що конструктор `Date()` містить багато опцій для створення дат та часу у певному форматі. Детальну інформацію про ці опції можна знайти в [офіційній документації](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date).

## Дивіться також
- [Офіційна документація TypeScript](https://www.typescriptlang.org/docs/)
- [Використання дат в TypeScript](https://www.digitalocean.com