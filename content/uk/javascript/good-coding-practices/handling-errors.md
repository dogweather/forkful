---
title:                "Обробка помилок"
date:                  2024-01-26T00:55:12.524266-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/javascript/handling-errors.md"
---

{{< edit_this_page >}}

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
