---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:13:56.221120-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết vào lỗi chuẩn (`stderr`) gửi thông điệp lỗi và chẩn đoán riêng biệt khỏi đầu ra chuẩn (`stdout`). Lập trình viên làm điều này để gỡ lỗi và ghi nhật ký lỗi mà không làm lộn xộn đầu ra chương trình thông thường.

## Làm thế nào:

Trong TypeScript, bạn có thể viết vào `stderr` sử dụng `console.error` hoặc `process.stderr.write`. Dưới đây là cả hai đang hoạt động:

```TypeScript
console.error("Đây là một thông điệp lỗi gửi đến stderr");

process.stderr.write("Đây là một thông điệp lỗi khác gửi đến stderr\n");
```

Mẫu đầu ra cho cả hai dòng:

```
Đây là một thông điệp lỗi gửi đến stderr
Đây là một thông điệp lỗi khác gửi đến stderr
```

## Sâu hơn

Truyền thống, việc tách biệt `stdout` và `stderr` cho phép người dùng Unix chỉ định đầu ra và lỗi vào các điểm đến khác nhau. Bạn có thể ghi nhật ký lỗi để phân tích trong khi vẫn có dữ liệu đầu ra sạch sẽ. Các phương án thay thế cho việc viết trực tiếp vào `stderr` bao gồm thư viện ghi nhật ký hay khung làm việc cung cấp nhiều kiểm soát và tính năng hơn. Về mặt thực thi, `console.error` bọc quanh `process.stderr.write` với khả năng định dạng bổ sung, do đó sử dụng `console.error` nói chung là tiện lợi hơn cho các thông điệp đơn giản.

## Xem thêm

- Tài liệu Node.js về console: https://nodejs.org/api/console.html
- Luồng chuẩn của quy trình Node.js: https://nodejs.org/api/process.html#process_process_stderr
- Thảo luận về `console.error` so với `process.stderr.write`: https://stackoverflow.com/questions/4976466/difference-between-process-stdout-write-and-console-log-in-node-js
