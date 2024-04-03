---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:23.678470-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: TypeScript kh\xF4ng \u0111i k\xE8m v\u1EDB\
  i REPL ri\xEAng c\u1EE7a m\xECnh. H\xE3y s\u1EED d\u1EE5ng `ts-node`, m\u1ED9t m\xF4\
  i tr\u01B0\u1EDDng th\u1EF1c thi TypeScript cho Node.js bao g\u1ED3m c\u1EA3 REPL.\u2026"
lastmod: '2024-03-13T22:44:36.321174-06:00'
model: gpt-4-0125-preview
summary: "TypeScript kh\xF4ng \u0111i k\xE8m v\u1EDBi REPL ri\xEAng c\u1EE7a m\xEC\
  nh."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Cách thực hiện:
TypeScript không đi kèm với REPL riêng của mình. Hãy sử dụng `ts-node`, một môi trường thực thi TypeScript cho Node.js bao gồm cả REPL.

Trước tiên, hãy cài đặt nó toàn cầu:
```bash
npm install -g ts-node
```

Khởi động REPL bằng cách gõ `ts-node` trên dòng lệnh của bạn:
```bash
ts-node
```

Dưới đây là một đoạn mã nhanh để thử:
```TypeScript
> let message: string = 'Xin chào, REPL!';
> console.log(message);
Xin chào, REPL!
> 
```
Để kết thúc phiên, nhấn `Ctrl+D`.

## Thảo Luận Sâu Hơn
Trong lịch sử, REPL đã nổi bật trong các ngôn ngữ như Lisp, cho phép đánh giá mã động. Khái niệm này đã lan rộng, trở thành một phần cốt lõi cho việc lập trình tương tác trong nhiều ngôn ngữ.

Đối với TypeScript, `ts-node` không phải là lựa chọn duy nhất của bạn. Các phương án khác bao gồm sử dụng Sân chơi TypeScript trong một trình duyệt web hoặc tận dụng các REPL dựa trên Node.js khác hỗ trợ TypeScript với các plugin phù hợp.

Về mặt triển khai, `ts-node` sử dụng API biên dịch TypeScript để chuyển mã ngay lập tức trước khi nó được Node.js thực thi. Điều này cung cấp phản hồi ngay lập tức và đặc biệt hữu ích để thử nghiệm các tính năng mới nhất của TypeScript mà không cần cài đặt rườm rà.

Một điều cần nhớ - mặc dù REPL tốt cho các bài kiểm tra nhanh, nó không thay thế việc viết mã truyền thống, có thể kiểm thử và bảo dưỡng được. Nó là một công cụ cho việc học và khám phá, không phải là thay thế cho các thực hành phát triển đúng đắn.

## Xem Thêm
- [Trang Chủ Chính Thức của TypeScript](https://www.typescriptlang.org/)
- [ts-node trên GitHub](https://github.com/TypeStrong/ts-node)
- [Tài Liệu REPL của Node.js](https://nodejs.org/api/repl.html)
- [Sân chơi TypeScript](https://www.typescriptlang.org/play)
