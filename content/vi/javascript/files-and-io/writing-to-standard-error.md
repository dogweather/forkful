---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:14:06.246726-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Theo l\u1ECBch s\u1EED, c\xE1c h\u1EC7\
  \ th\u1ED1ng gi\u1ED1ng Unix ph\xE2n bi\u1EC7t gi\u1EEFa \u0111\u1EA7u ra chu\u1EA9\
  n v\xE0 l\u1ED7i chu\u1EA9n \u0111\u1EC3 cho ph\xE9p x\u1EED l\xFD ri\xEAng bi\u1EC7\
  t c\xE1c tin nh\u1EAFn b\xECnh th\u01B0\u1EDDng v\xE0\u2026"
lastmod: '2024-04-05T22:50:51.473573-06:00'
model: gpt-4-0125-preview
summary: "Theo l\u1ECBch s\u1EED, c\xE1c h\u1EC7 th\u1ED1ng gi\u1ED1ng Unix ph\xE2\
  n bi\u1EC7t gi\u1EEFa \u0111\u1EA7u ra chu\u1EA9n v\xE0 l\u1ED7i chu\u1EA9n \u0111\
  \u1EC3 cho ph\xE9p x\u1EED l\xFD ri\xEAng bi\u1EC7t c\xE1c tin nh\u1EAFn b\xECnh\
  \ th\u01B0\u1EDDng v\xE0 tin nh\u1EAFn l\u1ED7i."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
