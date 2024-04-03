---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:21.834510-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong TypeScript, x\u1EED l\xFD l\u1ED7i th\u01B0\
  \u1EDDng li\xEAn quan \u0111\u1EBFn c\xE1c kh\u1ED1i `try`, `catch`, v\xE0 `finally`."
lastmod: '2024-03-13T22:44:36.329319-06:00'
model: gpt-4-0125-preview
summary: "Trong TypeScript, x\u1EED l\xFD l\u1ED7i th\u01B0\u1EDDng li\xEAn quan \u0111\
  \u1EBFn c\xE1c kh\u1ED1i `try`, `catch`, v\xE0 `finally`."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Làm thế nào:
Trong TypeScript, xử lý lỗi thường liên quan đến các khối `try`, `catch`, và `finally`.

```typescript
function riskyOperation() {
  throw new Error("Something went wrong!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Caught an error:", error.message);
  } finally {
    console.log("This always runs, error or not.");
  }
}

handleErrors();
```

Kết quả mẫu:

```
Caught an error: Something went wrong!
This always runs, error or not.
```

Ví dụ về Async với promises:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Mô phỏng lỗi
    reject("Failed miserably");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Caught async error:", error);
  }
}

handleAsyncErrors();
```

Kết quả mẫu:

```
Caught async error: Failed miserably
```

## Khám phá sâu hơn
Xử lý lỗi đã là một phần quan trọng của lập trình kể từ khi nó ra đời. Trong TypeScript, xây dựng dựa trên JavaScript, xử lý lỗi trở nên mạnh mẽ hơn với sự ra đời của async/await trong ECMAScript 2017. Trước đó, chúng ta thường dựa vào các hàm callback và promises để xử lý lỗi trong code bất đồng bộ.

Một lựa chọn thay thế cho `try/catch` trong TypeScript là sử dụng các khuôn khổ lỗi được cung cấp bởi các frameworks như React. Đối với xử lý phía server, chúng ta có thể sử dụng middleware trong các nền tảng như Express.js để tập trung quản lý lỗi.

Về mặt triển khai, TypeScript không có cơ chế xử lý lỗi riêng của mình nhưng lại dựa vào JavaScript. Các lớp lỗi tự tạo có thể mở rộng từ lớp `Error` để cung cấp thông tin lỗi mô tả rõ ràng hơn.

## Xem thêm
- [MDN về try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await trên MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Sử dụng Error Boundaries trong React](https://reactjs.org/docs/error-boundaries.html)
- [Xử lý lỗi trong Express.js](https://expressjs.com/en/guide/error-handling.html)
