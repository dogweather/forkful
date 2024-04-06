---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:39.477634-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Vi\u1EC7c n\u1ED1i chu\u1ED7i l\xE0 c\u01A1\
  \ b\u1EA3n; n\xF3 \u0111\xE3 xu\u1EA5t hi\u1EC7n t\u1EEB nh\u1EEFng ng\xE0y \u0111\
  \u1EA7u c\u1EE7a ng\xE0nh l\u1EADp tr\xECnh. V\u1EDBi TypeScript, m\u1EDF ra t\u1EEB\
  \ JavaScript, ch\xFAng ta \u0111\xE3 \u0111i xa\u2026"
lastmod: '2024-04-05T21:53:37.724638-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c n\u1ED1i chu\u1ED7i l\xE0 c\u01A1 b\u1EA3n; n\xF3 \u0111\xE3 xu\u1EA5\
  t hi\u1EC7n t\u1EEB nh\u1EEFng ng\xE0y \u0111\u1EA7u c\u1EE7a ng\xE0nh l\u1EADp\
  \ tr\xECnh."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cách thực hiện:
```TypeScript
let greeting: string = "Hello";
let target: string = "World";
let message: string = greeting + ", " + target + "!"; // sử dụng toán tử +
console.log(message); // Kết quả: Hello, World!

let anotherMessage: string = `${greeting}, ${target}!`; // sử dụng ký tự mẫu
console.log(anotherMessage); // Kết quả: Hello, World!
```

## Đi sâu hơn
Việc nối chuỗi là cơ bản; nó đã xuất hiện từ những ngày đầu của ngành lập trình. Với TypeScript, mở ra từ JavaScript, chúng ta đã đi xa khỏi các thao tác chuỗi cồng kềnh để đến với ký tự mẫu gọn gàng.

Trong lịch sử, bạn cần phải cẩn thận với việc nối chuỗi để không sử dụng quá nhiều bộ nhớ hoặc làm chậm trình duyệt. Các công cụ hiện đại được tối ưu hóa, nhưng hiệu quả vẫn quan trọng trong các ứng dụng quy mô lớn.

Có các phương án thay thế:
1. Mảng và `.join()`: Hữu ích khi bạn đang xử lý một danh sách các chuỗi.
2. Mô hình StringBuilder: Liên quan hơn đến các ngôn ngữ như Java hoặc C# nơi nó tối ưu hóa hiệu suất.

Về mặt thực hiện, TypeScript cuối cùng sẽ được biên dịch thành JavaScript. Ở tầm sâu, nó sử dụng các hàm và thao tác chuỗi do JavaScript cung cấp.

## Xem thêm
- Bạn có thể muốn xem qua tài liệu về Chuỗi của Mạng Lưới Nhà Phát Triển Mozilla [Tài liệu Chuỗi](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String) để hiểu sâu hơn về các phương pháp chuỗi.
- Đối với các câu hỏi cụ thể về chuỗi trong TypeScript, [tài liệu chính thức của TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string) là một tài liệu tham khảo nhanh.
