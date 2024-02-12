---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases:
- /vi/typescript/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:23.678470-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Một Vòng Lặp Đọc-Đánh Giá-In (REPL) là môi trường lập trình nhận các mã nhập từ người dùng, thực thi chúng và trả kết quả về cho người dùng. Lập trình viên sử dụng REPL để thử nghiệm nhanh chóng với đoạn mã, gỡ lỗi và học các tính năng ngôn ngữ mới mà không cần phải tạo một ứng dụng đầy đủ.

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
