---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:18.389888-07:00
description: "L\xE0m nh\u01B0 th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:37.160439-06:00'
model: gpt-4-0125-preview
summary: .
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

## Làm như thế nào:
```javascript
// Định nghĩa một hàm để tính diện tích hình chữ nhật
function calculateArea(width, height) {
  return width * height;
}

// Gọi hàm và in kết quả
let area = calculateArea(5, 3);
console.log(area); // Đầu ra: 15
```

```javascript
// Nhóm các chức năng liên quan sử dụng các hàm
function greet(name) {
  console.log(`Xin chào, ${name}!`);
}

function farewell(name) {
  console.log(`Tạm biệt, ${name}!`);
}

greet('Alice'); // Đầu ra: Xin chào, Alice!
farewell('Bob'); // Đầu ra: Tạm biệt, Bob!
```

## Đi sâu hơn
Lịch sử, các ngôn ngữ lập trình mệnh lệnh như các phiên bản đầu của BASIC hoặc Assembly thiếu sự trừu tượng mà các hàm cung cấp. Theo thời gian, khái niệm về mã mô-đun trong các ngôn ngữ như C đã giới thiệu ý tưởng rằng việc chia mã lệnh thành các đơn vị (hàm hoặc thủ tục) dẫn đến việc tổ chức tốt hơn và lô-gic rõ ràng hơn.

Trong JavaScript, ngoài các hàm bình thường, chúng ta có hàm mũi tên kể từ ES6 (2015) cung cấp một cú pháp ngắn gọn hơn và phù hợp cho các hàm không phải là phương thức.

Các phương án và cải tiến xung quanh việc tổ chức mã trong JavaScript bao gồm cách tiếp cận hướng đối tượng sử dụng các lớp, hoặc các mô hình lập trình hàm coi hàm như những công dân hạng nhất.

Về mặt thực hiện, các hàm JavaScript hỗ trợ đóng gói, cung cấp một cách để giữ quyền truy cập vào phạm vi của một hàm sau khi thực hiện, điều này mạnh mẽ cho việc đóng gói và tạo các hàm nhà máy, cùng các mẫu khác.

## Xem thêm
- MDN Web Docs về Hàm: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions
- Mẫu Thiết kế JavaScript: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Mã sạch JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
