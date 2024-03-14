---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:58.294264-07:00
description: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 \u0111o xem n\xF3 ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1c \u0111\
  \u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ho\u1EB7c gi\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.306702-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129\
  a l\xE0 \u0111o xem n\xF3 ch\u1EE9a bao nhi\xEAu k\xFD t\u1EF1. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c th\u1EF1c \u0111\
  \u1EA7u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ho\u1EB7c gi\u1EDBi\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tìm độ dài của một chuỗi nghĩa là đo xem nó chứa bao nhiêu ký tự. Lập trình viên thực hiện việc này để xác thực đầu vào, lặp qua các ký tự, hoặc giới hạn hiển thị văn bản, trong số các lý do khác.

## Làm thế nào:

Trong TypeScript, bạn có thể lấy độ dài của một chuỗi sử dụng thuộc tính `.length`. Dưới đây là một ví dụ nhanh:

```typescript
let greeting: string = "Hello, TypeScript!";
console.log(greeting.length); // Kết quả: 18
```

Đoạn mã này khai báo một biến chuỗi tên là `greeting` và sau đó ghi độ dài của nó ra console.

## Sâu hơn

Thuộc tính `.length` là một di sản từ JavaScript, tổ tiên của TypeScript. Đây là một cách đơn giản và được hỗ trợ phổ biến để lấy kích thước của một chuỗi.

Có những phương pháp khác, nhưng chúng thường làm phức tạp hóa vấn đề. Ví dụ, bạn có thể chuyển đổi chuỗi sang một mảng và đếm số phần tử:

```typescript
let greetingArray: string[] = Array.from(greeting);
console.log(greetingArray.length); // Kết quả: 18
```

Nhưng tại sao phải đi đường vòng? Thuộc tính `.length` hiệu quả vì chuỗi được lưu trữ dưới dạng mảng ký tự, vì vậy thông tin về độ dài sẵn có ngay lập tức.

Bây giờ, giả sử bạn đang xử lý các chuỗi từ các ngôn ngữ khác nhau. Bạn có thể gặp phải vấn đề với các ký tự đặc biệt. Phương pháp `.length` cơ bản đếm các đơn vị mã UTF-16, điều này có thể gây rắc rối cho các ký tự đòi hỏi hai đơn vị mã, được gọi là cặp thay thế. Trong trường hợp như vậy, thuộc tính `.length` có thể không cung cấp cho bạn số lượng ký tự thực tế, còn được gọi là điểm mã.

Dưới đây là cách bạn có thể xử lý các chuỗi với cặp thay thế:

```typescript
function countCodePoints(str: string): number {
    return Array.from(str).length;
}

let fancyGreeting: string = "Hello, 🌍!";
console.log(countCodePoints(fancyGreeting)); // Kết quả: 9
```

Hàm này xử lý các điểm mảnh của mã hóa chuỗi để đảm bảo mỗi ký tự, dù là một hoặc hai đơn vị mã, được đếm một cách chính xác.

## Xem thêm

- Sổ tay TypeScript về Chuỗi: [Sổ tay TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#strings)
- MDN Web Docs về thuộc tính độ dài Chuỗi: [String.prototype.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- Unicode và JavaScript: [JavaScript có một vấn đề về Unicode - Mathias Bynens](https://mathiasbynens.be/notes/javascript-unicode)
