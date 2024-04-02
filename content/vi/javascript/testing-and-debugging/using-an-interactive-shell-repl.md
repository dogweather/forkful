---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:26.273603-07:00
description: "Shell t\u01B0\u01A1ng t\xE1c, hay REPLs (V\xF2ng l\u1EB7p \u0110\u1ECD\
  c-\u0110\xE1nh gi\xE1-In), cho ph\xE9p b\u1EA1n ch\u1EA1y m\xE3 tr\xEAn m\xE1y bay,\
  \ ki\u1EC3m th\u1EED c\xE1c h\xE0m, thu\u1EADt to\xE1n, ho\u1EB7c ngh\u1ECBch ng\u1EE3\
  m v\u1EDBi \xFD t\u01B0\u1EDFng.\u2026"
lastmod: '2024-03-13T22:44:37.155497-06:00'
model: gpt-4-0125-preview
summary: "Shell t\u01B0\u01A1ng t\xE1c, hay REPLs (V\xF2ng l\u1EB7p \u0110\u1ECDc-\u0110\
  \xE1nh gi\xE1-In), cho ph\xE9p b\u1EA1n ch\u1EA1y m\xE3 tr\xEAn m\xE1y bay, ki\u1EC3\
  m th\u1EED c\xE1c h\xE0m, thu\u1EADt to\xE1n, ho\u1EB7c ngh\u1ECBch ng\u1EE3m v\u1EDB\
  i \xFD t\u01B0\u1EDFng.\u2026"
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cái gì và Tại sao?
Shell tương tác, hay REPLs (Vòng lặp Đọc-Đánh giá-In), cho phép bạn chạy mã trên máy bay, kiểm thử các hàm, thuật toán, hoặc nghịch ngợm với ý tưởng. Chúng là bảng nháp của việc lập trình, nhanh và bẩn, không cần thiết lập một môi trường phát triển đầy đủ.

## Làm thế nào:
Node.js đi kèm với một REPL có thể truy cập qua terminal. Mở lên, và bạn sẵn sàng lăn bánh. Dưới đây là một ví dụ:

```javascript
$ node
> let sum = (a, b) => a + b;
không xác định
> sum(5, 10);
15
> .exit
```

Dễ dàng, phải không? Định nghĩa biến, hàm, hoặc thực thi vòng lặp. Khi hoàn tất, `.exit` đưa bạn trở lại thế giới thực.

## Sâu hơn
REPLs đã tồn tại từ những năm 1960 – LISP đã khai sáng khái niệm này. Ý tưởng: cung cấp phản hồi ngay lập tức cho lập trình viên. Các phương thức thay thế? Ngoài REPL của Node.js, còn có các bảng điều khiển dựa trên trình duyệt như Chrome DevTools, các sân chơi trực tuyến như JSFiddle, hoặc các IDE đầy đủ chức năng như VSCode với sân chơi tương tác.

Trong hệ thống, quy trình làm việc của REPL thường bao gồm:
1. Đọc đầu vào
2. Biên dịch và thực thi mã
3. In ra kết quả
4. Quay lại vòng lặp

Đó là một chu trình đơn giản mà hiệu quả đã có ảnh hưởng lớn đến lập trình tương tác.

## Xem thêm
- [Tài liệu REPL của Node.js](https://nodejs.org/api/repl.html)
- [Giới thiệu về mô-đun JavaScript trên REPLs của Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
