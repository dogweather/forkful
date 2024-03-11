---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:52.878848-07:00
description: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1\
  i m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh\
  \ vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0m cho m\xE3\u2026"
lastmod: '2024-03-11T00:14:09.588930-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i\
  \ m\xE3 m\xE1y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh\
  \ vi b\xEAn ngo\xE0i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0m cho m\xE3\u2026"
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tái cấu trúc là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Lập trình viên thực hiện điều này để làm cho mã sạch hơn, dễ bảo trì hơn và giảm độ phức tạp, giúp mã dễ hiểu hơn đối với những người mới bắt đầu tìm hiểu.

## Làm thế nào:
Xem xét một hàm TypeScript đã qua thời hoàng kim - nó hơi bừa bộn và cần được chăm sóc một cách tỉ mỉ:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Sau khi tái cấu trúc, nó có thể trông như thế này:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

Ví dụ thứ hai chắc chắn hơn, tận dụng hệ thống kiểu của TypeScript với một `interface` để tránh lỗi thời gian chạy tiềm ẩn và cải thiện khả năng đọc.

## Sâu hơn
Tái cấu trúc không phải là một khái niệm hiện đại; nó phát triển cùng với lập trình, trở nên được chuẩn hóa hơn với sự ra đời của cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler vào năm 1999. Điều này cực kỳ quan trọng trong môi trường phát triển Agile, thúc đẩy việc thay đổi mã linh hoạt. Một số phương pháp thay thế cho việc tái cấu trúc thủ công bao gồm các công cụ tự động như TSLint hoặc chính máy chủ ngôn ngữ TypeScript có thể đề xuất hoặc thậm chí thực hiện một số tác vụ tái cấu trúc cho bạn. Chi tiết thực hiện thường liên quan đến việc nhận biết "mùi mã" như mã trùng lặp, phương thức dài, hoặc lớp lớn, và áp dụng mẫu để khắc phục—như trích xuất phương thức, di chuyển sang lớp phù hợp hơn, hoặc sử dụng cấu trúc đơn giản hơn. Những mẫu này là chìa khóa để hiểu về cách và lý do của việc tái cấu trúc.

## Xem thêm
- [Cuốn sách "Refactoring: Improving the Design of Existing Code" của Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint để phân tích mã tĩnh](https://palantir.github.io/tslint/)
- [Hiểu về Mùi Mã](https://refactoring.guru/refactoring/smells)
