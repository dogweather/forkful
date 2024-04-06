---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:07.323803-07:00
description: "\u041A\u0430\u043A: \u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\
  \u0430 \u043E\u0448\u0438\u0431\u043E\u043A \u0432 JavaScript \u0440\u0430\u0437\
  \u0432\u0438\u0432\u0430\u043B\u0430\u0441\u044C. \u0412 \u0441\u0442\u0430\u0440\
  \u044B\u0435 \u0432\u0440\u0435\u043C\u0435\u043D\u0430 (ES3, \u043F\u0440\u0438\
  \u043C\u0435\u0440\u043D\u043E 1999 \u0433\u043E\u0434) \u0443 \u043D\u0430\u0441\
  \ \u0431\u044B\u043B \u0442\u043E\u043B\u044C\u043A\u043E \u0431\u043B\u043E\u043A\
  \ `try-catch`. \u041D\u0435 \u043E\u0447\u0435\u043D\u044C \u0433\u0438\u0431\u043A\
  \u043E, \u043D\u043E \u0432\u044B\u043F\u043E\u043B\u043D\u044F\u043B\u043E\u2026"
lastmod: '2024-04-05T22:50:59.086590-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\
  \u0431\u043E\u043A \u0432 JavaScript \u0440\u0430\u0437\u0432\u0438\u0432\u0430\u043B\
  \u0430\u0441\u044C."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как:
Вот классический блок `try-catch`:

```javascript
try {
  // Код, который может вызвать ошибку
  let result = potentiallyRiskyOperation();
  console.log('Успех:', result);
} catch (error) {
  // Что делать, если возникла ошибка
  console.error('Упс:', error.message);
}
```

Пример вывода, когда ошибки не произошло:
```
Успех: 42
```

И когда произошла ошибка:
```
Упс: Что-то пошло не так
```

Для асинхронного кода, где используются промисы, используйте `try-catch` в асинхронной функции:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Данные получены:', data);
  } catch (error) {
    console.error('Ошибка при получении данных:', error.message);
  }
}

fetchData();
```

## Глубокое погружение
Обработка ошибок в JavaScript развивалась. В старые времена (ES3, примерно 1999 год) у нас был только блок `try-catch`. Не очень гибко, но выполняло свою функцию.

ES6 (2015) ввёл промисы и предоставил нам методы `.then()` и `.catch()`, позволяя более изящно обрабатывать асинхронные ошибки.

```javascript
fetch('https://api.example.com/data')
  .then(data => console.log('Данные получены:', data))
  .catch(error => console.error('Ошибка при получении данных:', error.message));
```

Что касается деталей реализации, когда возникает ошибка, движки JavaScript создают объект `Error` с полезными свойствами, такими как `message` и `stack`. Вы также можете создавать собственные типы ошибок, расширяя класс `Error` — это удобно для более сложных приложений.

Альтернативы? Вы могли бы игнорировать обработку ошибок (плохая идея), использовать колбэки с параметрами, где ошибка на первом месте (привет, стиль Node.js), или обратиться к библиотекам и фреймворкам, которые предлагают свои варианты.

## Смотрите также
Для дополнительной информации об обработке ошибок:

- MDN о try-catch: [MDN try...catch](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN async function](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Statements/async_function)
- Руководство по промисам: [MDN Promises](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Создание и генерация собственных ошибок: [MDN Error](https://developer.mozilla.org/ru/docs/Web/JavaScript/Reference/Global_Objects/Error)
