---
title:                "Обработка ошибок"
date:                  2024-01-28T23:59:16.445777-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обработка ошибок"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/typescript/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Обработка ошибок заключается в ожидании неожиданного; это то, как мы справляемся, когда в нашем коде что-то идет не так. Мы делаем это, чтобы избежать сбоев и обеспечить пользователям плавный опыт, даже когда происходит неожиданное.

## Как:
В TypeScript обработка ошибок часто включает блоки `try`, `catch` и `finally`.

```typescript
function riskyOperation() {
  throw new Error("Что-то пошло не так!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Ошибка перехвачена:", error.message);
  } finally {
    console.log("Это выполняется всегда, была ошибка или нет.");
  }
}

handleErrors();
```

Пример вывода:

```
Ошибка перехвачена: Что-то пошло не так!
Это выполняется всегда, была ошибка или нет.
```

Асинхронный пример с промисами:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Имитация ошибки
    reject("Катастрофическая неудача");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Асинхронная ошибка перехвачена:", error);
  }
}

handleAsyncErrors();
```

Пример вывода:

```
Асинхронная ошибка перехвачена: Катастрофическая неудача
```

## Глубже в Тему
Обработка ошибок является краеугольным камнем программирования с момента его зарождения. В TypeScript, который построен на основе JavaScript, обработка ошибок стала более надежной с введением async/await в ECMAScript 2017. До этого мы часто полагались на функции обратного вызова и промисы для обработки ошибок в асинхронном коде.

Альтернативой `try/catch` в TypeScript является использование границ ошибок, предоставляемых фреймворками, такими как React. Для серверной обработки мы можем использовать промежуточное ПО на платформах вроде Express.js для централизации управления ошибками.

С точки зрения реализации, TypeScript не имеет собственного механизма обработки ошибок, но опирается на механизм JavaScript. Пользовательские классы ошибок могут наследовать класс `Error`, чтобы предоставлять более описательную информацию об ошибках.

## Смотрите Также
- [MDN о try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await на MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Использование границ ошибок в React](https://reactjs.org/docs/error-boundaries.html)
- [Обработка ошибок в Express.js](https://expressjs.com/en/guide/error-handling.html)
