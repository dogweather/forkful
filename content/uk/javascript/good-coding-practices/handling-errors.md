---
date: 2024-01-26 00:55:12.524266-07:00
description: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\
  \u043E\u043A - \u0446\u0435 \u0442\u0435, \u044F\u043A \u0432\u0438 \u043A\u0435\
  \u0440\u0443\u0454\u0442\u0435 \u0441\u0438\u0442\u0443\u0430\u0446\u0456\u044F\u043C\
  \u0438, \u043A\u043E\u043B\u0438 \u0443 \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\
  \u043E\u0434\u0456 \u0432\u0441\u0435 \u0439\u0434\u0435 \u043D\u0435 \u0437\u0430\
  \ \u043F\u043B\u0430\u043D\u043E\u043C. \u0426\u0435 \u0432\u0430\u0436\u043B\u0438\
  \u0432\u043E, \u043E\u0441\u043A\u0456\u043B\u044C\u043A\u0438 \u0434\u043E\u043F\
  \u043E\u043C\u0430\u0433\u0430\u0454 \u0432\u0430\u0448\u0438\u043C \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0430\u043C \u043A\u043E\u0440\u0435\u043A\u0442\u043D\
  \u043E\u2026"
lastmod: '2024-03-13T22:44:50.006635-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\
  \u043E\u043A - \u0446\u0435 \u0442\u0435, \u044F\u043A \u0432\u0438 \u043A\u0435\
  \u0440\u0443\u0454\u0442\u0435 \u0441\u0438\u0442\u0443\u0430\u0446\u0456\u044F\u043C\
  \u0438, \u043A\u043E\u043B\u0438 \u0443 \u0432\u0430\u0448\u043E\u043C\u0443 \u043A\
  \u043E\u0434\u0456 \u0432\u0441\u0435 \u0439\u0434\u0435 \u043D\u0435 \u0437\u0430\
  \ \u043F\u043B\u0430\u043D\u043E\u043C."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Що і чому?

Обробка помилок - це те, як ви керуєте ситуаціями, коли у вашому коді все йде не за планом. Це важливо, оскільки допомагає вашим програмам коректно зупинятися та чітко інструктувати користувачів, замість того, щоб просто "вибухати і згоряти".

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
