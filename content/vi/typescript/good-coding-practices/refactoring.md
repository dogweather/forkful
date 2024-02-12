---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:05:52.878848-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
