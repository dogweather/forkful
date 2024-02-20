---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:06.246726-07:00
description: "Vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (stderr) l\xE0 vi\u1EC7c xu\u1EA5\
  t v\u0103n b\u1EA3n ra lu\u1ED3ng l\u1ED7i. N\xF3 t\xE1ch bi\u1EC7t \u0111\u1EA7\
  u ra b\xECnh th\u01B0\u1EDDng (stdout) kh\u1ECFi c\xE1c l\u1ED7i, cho ph\xE9p g\u1EE1\
  \ l\u1ED7i v\xE0 ph\xE2n t\xEDch log d\u1EC5\u2026"
lastmod: 2024-02-19 22:04:56.405538
model: gpt-4-0125-preview
summary: "Vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (stderr) l\xE0 vi\u1EC7c xu\u1EA5t\
  \ v\u0103n b\u1EA3n ra lu\u1ED3ng l\u1ED7i. N\xF3 t\xE1ch bi\u1EC7t \u0111\u1EA7\
  u ra b\xECnh th\u01B0\u1EDDng (stdout) kh\u1ECFi c\xE1c l\u1ED7i, cho ph\xE9p g\u1EE1\
  \ l\u1ED7i v\xE0 ph\xE2n t\xEDch log d\u1EC5\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Viết vào lỗi chuẩn (stderr) là việc xuất văn bản ra luồng lỗi. Nó tách biệt đầu ra bình thường (stdout) khỏi các lỗi, cho phép gỡ lỗi và phân tích log dễ dàng hơn.

## Cách thực hiện:

```javascript
// Viết một thông báo lỗi đơn giản vào stderr
console.error('Lỗi: Đã xảy ra sự cố');

// Ví dụ với đầu ra được định dạng
const errorCode = 404;
console.error(`Lỗi: Không tìm thấy trang - Mã ${errorCode}`);
```

Đầu ra mẫu:
```
Lỗi: Đã xảy ra sự cố
Lỗi: Không tìm thấy trang - Mã 404
```

## Sâu hơn
Theo lịch sử, các hệ thống giống Unix phân biệt giữa đầu ra chuẩn và lỗi chuẩn để cho phép xử lý riêng biệt các tin nhắn bình thường và tin nhắn lỗi. Trong khi `console.log` trong Javascript viết vào stdout, `console.error` cụ thể viết vào stderr.
Các phương án thay thế cho viết vào stderr bao gồm sử dụng `process.stderr.write()`, không bao gồm một ký tự dòng mới ở cuối, không giống như `console.error`.
Về cách thực hiện, khi viết script Node.js, đầu ra từ `console.error()` có thể được chuyển hướng riêng biệt từ `console.log()` khi thực thi một script từ dòng lệnh, điều này có thể hữu ích cho việc đăng nhập lỗi vào một tệp khác.

## Xem thêm
- MDN Web Docs về Console: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Tài liệu Node.js về `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Giải thích về stdout so với stderr: https://www.jstor.org/stable/25860673
