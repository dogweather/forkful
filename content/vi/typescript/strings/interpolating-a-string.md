---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:38.755164-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 n\u1ED9i suy m\u1ED9t chu\u1ED7\
  i trong TypeScript, b\u1EA1n s\u1EED d\u1EE5ng d\u1EA5u backtick `` ` `` v\xE0 c\xFA\
  \ ph\xE1p `${bi\u1EC3u th\u1EE9c}`."
lastmod: '2024-03-13T22:44:36.300333-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 n\u1ED9i suy m\u1ED9t chu\u1ED7i trong TypeScript, b\u1EA1\
  n s\u1EED d\u1EE5ng d\u1EA5u backtick `` ` `` v\xE0 c\xFA ph\xE1p `${bi\u1EC3u th\u1EE9\
  c}`."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Để nội suy một chuỗi trong TypeScript, bạn sử dụng dấu backtick `` ` `` và cú pháp `${biểu thức}`:

```TypeScript
let user = 'Charlie';
let age = 27;

// Nội suy chuỗi
let greeting = `Hi, I'm ${user} và tôi ${age} tuổi.`;

console.log(greeting);  // Kết quả: Hi, I'm Charlie và tôi 27 tuổi.
```

## Sâu hơn:
Nội suy chuỗi không phải là đặc điểm riêng của TypeScript; nó cũng có trong JavaScript kể từ ES6 và nhiều ngôn ngữ khác. Trước đây, chúng ta ghép chuỗi sử dụng toán tử `+`, trông như thế này:

```TypeScript
let greeting = 'Hi, I\'m ' + user + ' và tôi ' + age + 'tuổi.';
```

Phương pháp dùng `+` vẫn hoạt động nhưng cồng kềnh và khó đọc hơn, đặc biệt là với nhiều biến. Với nội suy, các mẫu/template trở nên sạch sẽ hơn và lỗi dễ tránh hơn.

Cái gì đang diễn ra bên dưới? Chuỗi nội suy là "syntactic sugar" — một cách đơn giản hóa để sử dụng tính năng phức tạp hơn được biết đến như "literals mẫu". Khi được biên dịch, cách nội suy thân thiện, dễ đọc của bạn được chuyển đổi thành định dạng mà máy tính JavaScript có thể hiểu, thường liên quan đến ghép chuỗi hoặc các phương pháp thao tác chuỗi khác.

Một phương án khác cho nội suy là sử dụng các hàm mẫu hoặc thư viện, nhưng cho hầu hết các trường hợp, nội suy với dấu backtick là công cụ tiện lợi nhất cho công việc.

## Xem Thêm:
- [Mạng Lưới Phát Triển Mozilla về Literals Mẫu](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [Tài liệu TypeScript](https://www.typescriptlang.org/docs/)
- [Các Tính Năng và Cú Pháp của ES6](http://es6-features.org/#StringInterpolation)
