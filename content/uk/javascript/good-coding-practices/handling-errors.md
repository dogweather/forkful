---
date: 2024-01-26 00:55:12.524266-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0441\u044C \u043A\u043B\u0430\u0441\u0438\u0447\u043D\u0438\u0439 \u0431\
  \u043B\u043E\u043A `try-catch`."
lastmod: '2024-03-13T22:44:50.006635-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0441\u044C \u043A\u043B\u0430\u0441\u0438\u0447\u043D\u0438\u0439\
  \ \u0431\u043B\u043E\u043A `try-catch`."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Як це зробити:
Ось класичний блок `try-catch`:

```javascript
try {
  // Код, який може викликати помилку
  let result = potentiallyRiskyOperation();
  console.log('Успіх:', result);
} catch (error) {
  // Що робити, якщо виникла помилка
  console.error('Ой:', error.message);
}
```

Приклад виводу, коли помилки не відбувається:
```
Успіх: 42
```

І коли сталася помилка:
```
Ой: Щось пішло не так
```

Для асинхронного коду, де використовуються проміси, використовуйте `try-catch` у функції `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Дані отримано:', data);
  } catch (error) {
    console.error('Помилка отримання даних:', error.message);
  }
}

fetchData();
```

## Детальніше
Обробка помилок у JavaScript розвивалася. Колись (ES3, приблизно 1999 рік) у нас був лише блок `try-catch`. Не дуже гнучкий, але виконував свою роботу.

ES6 (2015) ввів проміси і надав нам `.then()` та `.catch()`, що дозволили нам більш елегантно обробляти асинхронні помилки.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Дані отримано:', data))
  .catch(error => console.error('Помилка отримання даних:', error.message));
```

Що стосується деталей реалізації, коли помилка викидається, движки JavaScript створюють об'єкт `Error` з корисними властивостями, такими як `message` та `stack`. Ви також можете створювати свої типи помилок, розширюючи клас `Error` – зручно для більш складних додатків.

Альтернативи? Можна ігнорувати обробку помилок (погана ідея), використовувати функції зворотного виклику з параметрами, що містять помилку на першому місці (привіт, стиль Node.js), або використовувати більш витончені бібліотеки та фреймворки, які пропонують своє бачення.

## Дивіться також
Більше про обробку помилок:

- MDN про try-catch: [MDN try...catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/async_function)
- Посібник щодо Промісів: [MDN Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Створення та кидання налаштованих помилок: [MDN Error](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error)
