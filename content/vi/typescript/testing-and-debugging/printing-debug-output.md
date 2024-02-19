---
aliases:
- /vi/typescript/printing-debug-output/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:04.610943-07:00
description: "Vi\u1EC7c in \u1EA5n th\xF4ng tin debug gi\u1ED1ng nh\u01B0 chi\u1EBF\
  c \u0111\xE8n pin trong ng\xF5 t\u1ED1i c\u1EE7a m\xE3 l\u1EC7nh; n\xF3 gi\xFAp\
  \ b\u1EA1n ph\xE1t hi\u1EC7n l\u1ED7i b\u1EB1ng c\xE1ch cho ph\xE9p b\u1EA1n nh\xEC\
  n th\u1EA5u nh\u1EEFng g\xEC m\xE3 l\u1EC7nh\u2026"
lastmod: 2024-02-18 23:08:50.418193
model: gpt-4-0125-preview
summary: "Vi\u1EC7c in \u1EA5n th\xF4ng tin debug gi\u1ED1ng nh\u01B0 chi\u1EBFc \u0111\
  \xE8n pin trong ng\xF5 t\u1ED1i c\u1EE7a m\xE3 l\u1EC7nh; n\xF3 gi\xFAp b\u1EA1\
  n ph\xE1t hi\u1EC7n l\u1ED7i b\u1EB1ng c\xE1ch cho ph\xE9p b\u1EA1n nh\xECn th\u1EA5\
  u nh\u1EEFng g\xEC m\xE3 l\u1EC7nh\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Những gì & Tại sao?
Việc in ấn thông tin debug giống như chiếc đèn pin trong ngõ tối của mã lệnh; nó giúp bạn phát hiện lỗi bằng cách cho phép bạn nhìn thấu những gì mã lệnh của mình đang thực hiện tại thời gian chạy. Các lập trình viên làm điều này vì, chà, chúng ta là con người và mã lệnh của mình không luôn hoàn hảo ngay từ lần thử đầu tiên.

## Cách thực hiện:
Muốn in thông tin debug trong TypeScript? Các phương thức của Console chính là lựa chọn của bạn. Xem `console.log`, `console.error`, và những người bạn khác được áp dụng:

```TypeScript
// Log cơ bản
console.log('Này Má, Con đang debug đây!');

// Nhóm các log
console.group('Thông tin Người Dùng');
console.log('Tên: John Doe');
console.log('Tuổi: 34');
console.groupEnd();

// Bảng
console.table([{ a: 1, b: 'Y' }, { a: 'Z', b: 2 }]);

// Xuất thông tin lỗi
console.error('Ối! Có điều gì đó đã sai.');

// Xuất thông tin cảnh báo
console.warn('Đây là một cảnh báo.');

// Một thông tin debug
console.debug('Đây là một thông điệp debug.');
```

Các Đầu ra Mẫu:
```
Này Má, Con đang debug đây!
Thông tin Người Dùng
    Tên: John Doe
    Tuổi: 34
(index) a  b
0       1  "Y"
1       "Z" 2
Ối! Có điều gì đó đã sai.
Đây là một cảnh báo.
Đây là một thông điệp debug.
```

## Sâu hơn
Ngày xưa, chúng ta có `alert()` - nó chen vào mặt người và chặn công việc cho đến khi được xử lý. Bây giờ, các phương thức `console` thống trị. Chúng kém phô trương hơn và đi kèm với siêu năng lực: phân loại thông điệp, in bảng, hoặc tạo kiểu cho đầu ra.

Có lựa chọn khác không? Chắc chắn rồi. Bạn có thể viết vào một tệp hoặc gửi tin nhắn qua mạng cho việc log từ xa. Đối với trình duyệt, các công cụ như DevTools của Chrome giúp bạn kiểm soát tốt hơn mức độ log và định dạng.

Về mặt thực hiện, `console` trong TypeScript trở thành JavaScript khi chạy, và đó là nơi diễn ra tất cả những hành động thực sự. Các kiểu TypeScript tinh tế không thay đổi cuộc chơi ở đây—nó vẫn là `console` truyền thống ở bên dưới, dù là trên trình duyệt hoặc Node.

## Xem Thêm
- [MDN Web Docs về Console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Tài liệu Console của Node.js](https://nodejs.org/api/console.html)
- [Sổ tay TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
