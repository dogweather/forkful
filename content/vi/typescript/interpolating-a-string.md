---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:38.755164-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
