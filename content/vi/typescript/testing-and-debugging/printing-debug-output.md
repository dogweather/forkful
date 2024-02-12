---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-01-28T22:05:04.610943-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
