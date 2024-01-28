---
title:                "处理错误"
date:                  2024-01-26T00:58:15.719236-07:00
model:                 gpt-4-1106-preview
simple_title:         "处理错误"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/handling-errors.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
处理错误关乎预料之外的状况；这是我们在代码出问题时的管理方式。我们这么做是为了避免崩溃，并在出现意外时仍能给用户提供流畅的体验。

## 如何操作：
在TypeScript中，处理错误通常涉及 `try`、`catch` 和 `finally` 块。

```typescript
function riskyOperation() {
  throw new Error("出错了！");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("捕获到错误：", error.message);
  } finally {
    console.log("无论有没有错误，这都会执行。");
  }
}

handleErrors();
```

示例输出：

```
捕获到错误：出错了！
无论有没有错误，这都会执行。
```

带有 promises 的异步示例：

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // 模拟一个错误
    reject("惨败");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("捕获到异步错误：", error);
  }
}

handleAsyncErrors();
```

示例输出：

```
捕获到异步错误：惨败
```

## 深入探讨
自编程诞生以来，错误处理一直是编程的基石。在TypeScript中，它基于JavaScript，通过ECMAScript 2017引入的async/await，错误处理变得更加健壮。在此之前，我们常常依赖回调函数和promise来处理异步代码中的错误。

在TypeScript中，除了`try/catch`，还有一个替代方案是使用像React这样的框架提供的错误边界。对于服务端处理，我们可以在如Express.js这样的平台中使用中间件来集中管理错误。

在实现方面，TypeScript没有自己的错误处理机制，而是依赖JavaScript的。自定义错误类可以扩展`Error`类，以提供更具描述性的错误信息。

## 另见
- [MDN上的try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [MDN上的Async/Await](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [在React中使用错误边界](https://reactjs.org/docs/error-boundaries.html)
- [Express.js错误处理](https://expressjs.com/en/guide/error-handling.html)
