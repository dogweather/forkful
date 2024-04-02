---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:11.593838-07:00
description: "N\u1ED9i suy chu\u1ED7i l\xE0 c\xE1ch \u0111\u1EC3 nh\xFAng tr\u1EF1\
  c ti\u1EBFp c\xE1c bi\u1EBFn v\xE0o b\xEAn trong m\u1ED9t chu\u1ED7i. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 n\u1ED1i c\xE1c bi\u1EBFn v\xE0\
  \ chu\u1ED7i m\u1ED9t c\xE1ch hi\u1EC7u qu\u1EA3, l\xE0m cho\u2026"
lastmod: '2024-03-13T22:44:37.134294-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i l\xE0 c\xE1ch \u0111\u1EC3 nh\xFAng tr\u1EF1c ti\u1EBF\
  p c\xE1c bi\u1EBFn v\xE0o b\xEAn trong m\u1ED9t chu\u1ED7i. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 n\u1ED1i c\xE1c bi\u1EBFn v\xE0 chu\u1ED7\
  i m\u1ED9t c\xE1ch hi\u1EC7u qu\u1EA3, l\xE0m cho\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Là gì & Tại sao?

Nội suy chuỗi là cách để nhúng trực tiếp các biến vào bên trong một chuỗi. Lập trình viên sử dụng nó để nối các biến và chuỗi một cách hiệu quả, làm cho code dễ đọc và dễ bảo trì hơn.

## Làm thế nào:

Trong JavaScript, nội suy chuỗi thường được thực hiện bằng cách sử dụng các literal mẫu. Dưới đây là cách bạn có thể làm điều đó:

```javascript
const name = 'Alice';
const message = `Xin chào, ${name}! Hôm nay bạn thế nào?`;
console.log(message); // Đầu ra: Xin chào, Alice! Hôm nay bạn thế nào?
```

Bạn cũng có thể thực hiện các phép toán bên trong các vị trí giữ chỗ:

```javascript
const a = 10;
const b = 5;
console.log(`Mười nhân năm là ${a * b}.`); // Đầu ra: Mười nhân năm là 50.
```

## Sâu hơn nữa

Về mặt lịch sử, nội suy chuỗi không hẳn là điều dễ dàng trong JavaScript. Trước ES6 (ECMAScript 2015), việc nối chuỗi thường được thực hiện bằng cách sử dụng toán tử `+`:

```javascript
var name = 'Bob';
var message = 'Xin chào, ' + name + '! Hôm nay bạn thế nào?';
```

Với sự ra đời của ES6, các literal mẫu (nằm giữa dấu ngoặc ngược \` \`) xuất hiện, mang theo cú pháp dễ dàng hơn với các vị trí giữ chỗ `${}`.

Các phương pháp thay thế cho nội suy chuỗi bao gồm nối chuỗi bằng toán tử `+` và phương thức `concat()`, hoặc sử dụng các hàm giống như `sprintf` từ thư viện của bên thứ ba.

Hiệu suất của các literal mẫu nói chung tương đương với những phương pháp cũ hơn. Tuy nhiên, khả năng đọc và khả năng bao gồm biểu thức (như `${a * b}`) trong chuỗi làm cho các literal mẫu trở thành lựa chọn mạnh mẽ cho các nhà phát triển.

## Xem thêm

- MDN về Literal Mẫu: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- Nối chuỗi trong JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/String_Operators
- Lịch sử mô-đun JavaScript "ECMAScript": https://www.ecma-international.org/publications-and-standards/standards/ecma-262/
