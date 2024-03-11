---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:21.834510-07:00
description: "X\u1EED l\xFD l\u1ED7i l\xE0 vi\u1EC7c d\u1EF1 \u0111o\xE1n nh\u1EEF\
  ng \u0111i\u1EC1u b\u1EA5t ng\u1EDD; \u0111\xF3 l\xE0 c\xE1ch ch\xFAng ta qu\u1EA3\
  n l\xFD khi c\xF3 \u0111i\u1EC1u g\xEC \u0111\xF3 kh\xF4ng \u0111\xFAng trong code\
  \ c\u1EE7a m\xECnh. Ch\xFAng ta l\xE0m v\u1EADy \u0111\u1EC3 tr\xE1nh\u2026"
lastmod: '2024-03-11T00:14:09.587637-06:00'
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i l\xE0 vi\u1EC7c d\u1EF1 \u0111o\xE1n nh\u1EEFng \u0111\
  i\u1EC1u b\u1EA5t ng\u1EDD; \u0111\xF3 l\xE0 c\xE1ch ch\xFAng ta qu\u1EA3n l\xFD\
  \ khi c\xF3 \u0111i\u1EC1u g\xEC \u0111\xF3 kh\xF4ng \u0111\xFAng trong code c\u1EE7\
  a m\xECnh. Ch\xFAng ta l\xE0m v\u1EADy \u0111\u1EC3 tr\xE1nh\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xử lý lỗi là việc dự đoán những điều bất ngờ; đó là cách chúng ta quản lý khi có điều gì đó không đúng trong code của mình. Chúng ta làm vậy để tránh crash và để người dùng có trải nghiệm mượt mà, ngay cả khi điều bất ngờ xảy ra.

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
