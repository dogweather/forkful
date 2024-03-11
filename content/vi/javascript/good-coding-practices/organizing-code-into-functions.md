---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:18.389888-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m chia nh\u1EEF\
  ng nhi\u1EC7m v\u1EE5 ra th\xE0nh c\xE1c ph\u1EA7n c\xF3 th\u1EC3 t\xE1i s\u1EED\
  \ d\u1EE5ng, l\xE0m cho m\xE3 l\u1EC7nh tr\u1EDF n\xEAn g\u1ECDn g\xE0ng v\xE0 d\u1EC5\
  \ b\u1EA3o tr\xEC h\u01A1n. Ch\xFAng ta l\xE0m\u2026"
lastmod: '2024-03-11T00:14:10.467802-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m chia nh\u1EEFng\
  \ nhi\u1EC7m v\u1EE5 ra th\xE0nh c\xE1c ph\u1EA7n c\xF3 th\u1EC3 t\xE1i s\u1EED\
  \ d\u1EE5ng, l\xE0m cho m\xE3 l\u1EC7nh tr\u1EDF n\xEAn g\u1ECDn g\xE0ng v\xE0 d\u1EC5\
  \ b\u1EA3o tr\xEC h\u01A1n. Ch\xFAng ta l\xE0m\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Tổ chức mã lệnh thành các hàm chia những nhiệm vụ ra thành các phần có thể tái sử dụng, làm cho mã lệnh trở nên gọn gàng và dễ bảo trì hơn. Chúng ta làm điều này để giảm lặp lại, làm cho việc kiểm thử dễ dàng hơn, và cải thiện tính dễ đọc.

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
