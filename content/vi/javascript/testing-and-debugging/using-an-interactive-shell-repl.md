---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:26.273603-07:00
description: "L\xE0m th\u1EBF n\xE0o: Node.js \u0111i k\xE8m v\u1EDBi m\u1ED9t REPL\
  \ c\xF3 th\u1EC3 truy c\u1EADp qua terminal. M\u1EDF l\xEAn, v\xE0 b\u1EA1n s\u1EB5\
  n s\xE0ng l\u0103n b\xE1nh. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5\
  ."
lastmod: '2024-03-13T22:44:37.155497-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u0111i k\xE8m v\u1EDBi m\u1ED9t REPL c\xF3 th\u1EC3 truy c\u1EAD\
  p qua terminal."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

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
