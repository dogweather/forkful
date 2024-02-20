---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:38.755164-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n nh\xFAng c\xE1c bi\u1EBF\
  n v\xE0 bi\u1EC3u th\u1EE9c v\xE0o trong chu\u1ED7i. N\xF3 gi\xFAp cho code c\u1EE7\
  a b\u1EA1n d\u1EC5 \u0111\u1ECDc v\xE0 linh ho\u1EA1t - kh\xF4ng c\u1EA7n d\u1EA5\
  u c\u1ED9ng, kh\xF4ng r\u1EAFc r\u1ED1i,\u2026"
lastmod: 2024-02-19 22:04:55.448891
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n nh\xFAng c\xE1c bi\u1EBFn v\xE0\
  \ bi\u1EC3u th\u1EE9c v\xE0o trong chu\u1ED7i. N\xF3 gi\xFAp cho code c\u1EE7a b\u1EA1\
  n d\u1EC5 \u0111\u1ECDc v\xE0 linh ho\u1EA1t - kh\xF4ng c\u1EA7n d\u1EA5u c\u1ED9\
  ng, kh\xF4ng r\u1EAFc r\u1ED1i,\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Nội suy chuỗi cho phép bạn nhúng các biến và biểu thức vào trong chuỗi. Nó giúp cho code của bạn dễ đọc và linh hoạt - không cần dấu cộng, không rắc rối, không phiền phức.

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
