---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:11.593838-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong JavaScript, n\u1ED9i suy chu\u1ED7i th\u01B0\
  \u1EDDng \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5\
  ng c\xE1c literal m\u1EABu. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n c\xF3\
  \ th\u1EC3 l\xE0m \u0111i\u1EC1u \u0111\xF3."
lastmod: '2024-03-13T22:44:37.134294-06:00'
model: gpt-4-0125-preview
summary: "Trong JavaScript, n\u1ED9i suy chu\u1ED7i th\u01B0\u1EDDng \u0111\u01B0\u1EE3\
  c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng c\xE1c literal m\u1EAB\
  u."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
