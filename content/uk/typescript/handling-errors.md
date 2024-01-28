---
title:                "Обробка помилок"
date:                  2024-01-26T00:59:13.751300-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обробка помилок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## Що і Чому?
Обробка помилок полягає в очікуванні непередбачуваного; це те, як ми управляємо ситуаціями, коли щось іде не так у нашому коді. Ми це робимо, щоб уникнути збоїв та забезпечити користувачам плавну роботу, навіть коли стається несподіване.

## Як зробити:
У TypeScript обробка помилок часто включає блоки `try`, `catch`, та `finally`.

```typescript
function riskyOperation() {
  throw new Error("Щось пішло не так!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Помилка виловлена:", error.message);
  } finally {
    console.log("Це завжди виконується, з помилкою чи без.");
  }
}

handleErrors();
```

Приклад виводу:

```
Помилка виловлена: Щось пішло не так!
Це завжди виконується, з помилкою чи без.
```

Асинхронний приклад з обіцянками (promises):

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Симуляція помилки
    reject("Зазнала краху");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Асинхронна помилка виловлена:", error);
  }
}

handleAsyncErrors();
```

Приклад виводу:

```
Асинхронна помилка виловлена: Зазнала краху
```

## Поглибленний огляд
Обробка помилок була одним з кутових каменів програмування з самого початку. У TypeScript, який базується на JavaScript, обробка помилок стала більш надійною з введенням async/await у ECMAScript 2017. До цього ми часто покладалися на функції зворотного виклику та обіцянки для обробки помилок в асинхронному коді.

Альтернативою `try/catch` у TypeScript є використання меж обробки помилок, які надають фреймворки на кшталт React. Для обробки помилок на сервері можна використовувати проміжне програмне забезпечення на платформах на кшталт Express.js для централізації управління помилками.

З точки зору реалізації, TypeScript не має власного механізму обробки помилок, а покладається на механізм JavaScript. Класи помилок користувача можуть розширювати клас `Error`, щоб надати більш детальну інформацію про помилку.

## Дивіться також
- [MDN про try/catch](https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await на MDN](https://developer.mozilla.org/uk/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Використання меж обробки помилок у React](https://uk.reactjs.org/docs/error-boundaries.html)
- [Обробка помилок у Express.js](https://expressjs.com/en/guide/error-handling.html)
