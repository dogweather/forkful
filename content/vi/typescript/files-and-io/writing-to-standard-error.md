---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:56.221120-07:00
description: "Vi\u1EC7c vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) g\u1EEDi th\xF4\
  ng \u0111i\u1EC7p l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n ri\xEAng bi\u1EC7t kh\u1ECF\
  i \u0111\u1EA7u ra chu\u1EA9n (`stdout`). L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 g\u1EE1 l\u1ED7i v\xE0 ghi\u2026"
lastmod: '2024-03-13T22:44:36.341111-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) g\u1EEDi th\xF4\
  ng \u0111i\u1EC7p l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n ri\xEAng bi\u1EC7t kh\u1ECF\
  i \u0111\u1EA7u ra chu\u1EA9n (`stdout`). L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 g\u1EE1 l\u1ED7i v\xE0 ghi\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
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
