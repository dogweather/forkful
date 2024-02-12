---
title:                "Обработка ошибок"
aliases: - /ru/javascript/handling-errors.md
date:                  2024-01-28T23:59:07.323803-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обработка ошибок"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/javascript/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Обработка ошибок — это способ управления тем, как ваш код ведёт себя, когда что-то идёт не так. Это ключевой момент, потому что позволяет вашим программам завершать выполнение корректно и даёт пользователям чёткие инструкции, вместо того чтобы просто "вылетать" и завершаться некорректно.

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
