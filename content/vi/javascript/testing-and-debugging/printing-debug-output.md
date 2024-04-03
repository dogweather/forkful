---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:58.815004-07:00
description: "Vi\u1EC7c in th\xF4ng tin g\u1EE1 l\u1ED7i trong JavaScript li\xEAn\
  \ quan \u0111\u1EBFn vi\u1EC7c hi\u1EC3n th\u1ECB c\xE1c bi\u1EBFn, l\u1ED7i, ho\u1EB7\
  c b\u1EA5t k\u1EF3 th\xF4ng tin n\xE0o kh\xE1c gi\xFAp hi\u1EC3u r\xF5 nh\u1EEF\
  ng g\xEC m\xE3 c\u1EE7a b\u1EA1n \u0111ang\u2026"
lastmod: '2024-03-13T22:44:37.156721-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c in th\xF4ng tin g\u1EE1 l\u1ED7i trong JavaScript li\xEAn quan\
  \ \u0111\u1EBFn vi\u1EC7c hi\u1EC3n th\u1ECB c\xE1c bi\u1EBFn, l\u1ED7i, ho\u1EB7\
  c b\u1EA5t k\u1EF3 th\xF4ng tin n\xE0o kh\xE1c gi\xFAp hi\u1EC3u r\xF5 nh\u1EEF\
  ng g\xEC m\xE3 c\u1EE7a b\u1EA1n \u0111ang l\xE0m ch\u1EC9 qua m\u1ED9t c\xE1i nh\xEC\
  n."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Gì và Tại sao?

Việc in thông tin gỡ lỗi trong JavaScript liên quan đến việc hiển thị các biến, lỗi, hoặc bất kỳ thông tin nào khác giúp hiểu rõ những gì mã của bạn đang làm chỉ qua một cái nhìn. Lập trình viên làm điều này để bắt lỗi, hiểu luồng thực thi, và đảm bảo mã đang làm những gì nó cần làm.

## Làm thế nào:

JavaScript làm cho việc in thông tin gỡ lỗi trở nên cực kỳ dễ dàng sử dụng `console.log()`. Dưới đây là cách:

```javascript
console.log('Xin chào, thế giới gỡ lỗi!');

let số = 42;
console.log('Số là:', số);

function add(a, b) {
  console.log(`Đang cộng ${a} + ${b}`);
  return a + b;
}

let kết_quả = add(3, 4);
console.log('Kết quả:', kết_quả);
```

Mẫu đầu ra trong bảng điều khiển trình duyệt của bạn hoặc terminal Node.js sẽ trông như thế này:

```
Xin chào, thế giới gỡ lỗi!
Số là: 42
Đang cộng 3 + 4
Kết quả: 7
```

## Sâu hơn nữa

Phương thức `console.log()` đến từ Console API, đã là người bạn gỡ lỗi trong các môi trường trình duyệt và Node.js từ rất lâu. Nhưng không chỉ có `log`; bạn còn có `console.warn()`, `console.error()`, và `console.info()`, tất cả đều xuất ra thông điệp với các cấp độ nghiêm trọng khác nhau.

Từ lâu, các nhà phát triển sử dụng `alert()` để gỡ lỗi, nhưng điều đó nhanh chóng trở nên nhàm chán - nó chặn tương tác của người dùng bằng cách bật lên một hộp thoại.

Còn có `console.dir()` cung cấp cho bạn một cái nhìn giống JSON về một đối tượng, tiện lợi cho việc kiểm tra sâu. Nếu bạn muốn theo dõi xem điều gì đó mất bao lâu, `console.time()` và `console.timeEnd()` là bạn bè của bạn.

Dành cho những ai yêu thích một đầu ra sạch sẽ, `console.table()` hiển thị dữ liệu dưới dạng bảng gọn gàng. Và khi bạn đi xa hơn so với việc gỡ lỗi đơn giản và bước vào lĩnh vực hiệu suất, Console API có còn nhiều công cụ hơn nữa như `console.trace()` cho thông tin ngăn xếp cuộc gọi, `console.profile()` cho phân tích hiệu suất, và nhiều thứ khác.

Cách các phương thức `console` được triển khai có thể thay đổi giữa các môi trường JavaScript, nhưng bản chất vẫn giữ nguyên: chúng giúp các nhà phát triển hiểu rõ những gì đang xảy ra bên dưới một cách nhanh chóng và mà không mất nhiều công sức.

## Xem thêm

- MDN Web Docs về Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- Tài liệu `console` của Node.js: https://nodejs.org/api/console.html
- Hướng dẫn về các lệnh bảng điều khiển: https://getfirebug.com/wiki/index.php/Console_API
