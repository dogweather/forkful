---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:25.860378-07:00
description: ''
lastmod: '2024-03-13T22:44:37.133206-06:00'
model: gpt-4-0125-preview
summary: .
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Tìm kiếm và thay thế văn bản với Javascript

## Là gì & Tại sao?
Tìm kiếm và thay thế văn bản có nghĩa là tìm các chuỗi con cụ thể và thay thế chúng bằng cái mới. Tại sao phải làm thế? Nó xuất hiện mọi nơi: sửa lỗi đánh máy trong tài liệu, tinh chỉnh code, hoặc chỉnh sửa dữ liệu hàng loạt.

## Cách thực hiện:
Trong JavaScript, `String.prototype.replace()` là lựa chọn hàng đầu. Chỉ cần truyền vào một chuỗi hoặc regex và chuỗi thay thế. Dưới đây là cách nhanh và ngắn gọn:

```javascript
let str = "Tôi yêu thích lập trình bằng JavaScript!";
let newStr = str.replace("JavaScript", "TypeScript");
console.log(newStr); // Xuất ra: Tôi yêu thích lập trình bằng TypeScript!
```

Bây giờ, với regex để thay thế toàn bộ:

```javascript
let story = "Con cáo nhanh nhẹn nhảy qua chú chó lười. Con cáo rất thông minh.";
let newStory = story.replace(/cáo/g, "mèo");
console.log(newStory); // Xuất ra: Con mèo nhanh nhẹn nhảy qua chú chó lười. Con mèo rất thông minh.
```

## Sâu hơn nữa
Nhìn lại, `String.prototype.replace()` đã có trong JS từ những ngày đầu—từ thời Netscape 2. Giờ đây, ES6 đã mang đến cho chúng ta template literals và arrow functions, đã làm cho việc viết code ngắn gọn và dễ đọc hơn khi sử dụng regex.

Có cách thay thế không? Chắc chắn rồi. Nếu bạn làm việc với xử lý văn bản quy mô lớn, bạn có thể chuyển sang sử dụng luồng của Node.js hoặc tận dụng các thư viện bên ngoài để xử lý các mẫu phức tạp, hiệu quả và hiệu suất.

Về việc triển khai, việc sử dụng `replace()` một mình là đơn giản. Nhưng các mẫu regex có thể trở nên phức tạp. Bắt đầu từ những điều dễ dàng, học các ký tự đặc biệt (`.` khớp với bất kỳ ký tự nào, `*` cho các mẫu lặp lại), và kiểm tra với các công cụ như regex101.

## Xem thêm
- Tài liệu về replace của MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 để kiểm tra biểu thức: https://regex101.com/
- Thông tin về regex trong JavaScript: https://javascript.info/regexp-introduction
