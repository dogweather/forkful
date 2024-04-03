---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:42.010143-07:00
description: "L\xE0m th\u1EBF n\xE0o: K\xEDch ho\u1EA1t REPL b\u1EB1ng c\xE1ch m\u1EDF\
  \ m\u1ED9t terminal v\xE0 ch\u1EA1y `swift`. G\xF5 m\xE3 tr\u1EF1c ti\u1EBFp v\xE0\
  \ nh\u1EA5n Enter \u0111\u1EC3 ch\u1EA1y n\xF3. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5."
lastmod: '2024-03-13T22:44:37.099404-06:00'
model: gpt-4-0125-preview
summary: "K\xEDch ho\u1EA1t REPL b\u1EB1ng c\xE1ch m\u1EDF m\u1ED9t terminal v\xE0\
  \ ch\u1EA1y `swift`."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Làm thế nào:
Kích hoạt REPL bằng cách mở một terminal và chạy `swift`. Gõ mã trực tiếp và nhấn Enter để chạy nó. Dưới đây là một ví dụ:

```Swift
1> let greeting = "Xin chào, REPL!"
greeting: String = "Xin chào, REPL!"
2> print(greeting)
Xin chào, REPL!
```

Thoát với `:quit` hoặc `Control-D`.

## Sâu hơn
Gốc rễ của REPL trải dài về tận các bộ thông dịch Lisp trong những năm '60. REPL của Swift đặt trên LLVM, một khung công cụ biên dịch mạnh mẽ, cung cấp nhiều hơn là chỉ giải thích cơ bản - nó là một công cụ đầy đủ với tính năng tự động hoàn thành, gỡ lỗi, và hơn thế nữa. REPL thích hợp cho việc học hoặc tạo mẫu, nhưng nó không phải là một môi trường phát triển độc lập. Một số người thích sử dụng Playgrounds trong Xcode cho một cách tiếp cận đồ họa, dựa trên tập tin hơn, trong khi những người khác giữ lối chỉnh sửa và chạy kịch bản truyền thống.

Bên trong, REPL của Swift biên dịch mã đến ngôn ngữ máy và thực thi nó một cách động, đó là lý do tại sao nó tương đối nhanh. Nó cũng có thể truy cập vào bất kỳ mô-đun Swift biên dịch nào, hoặc thậm chí là thư viện C, làm cho nó khá mạnh mẽ. Tuy nhiên, lưu ý, không phải mọi thứ đều hoạt động hoàn hảo trong REPL; một số tính năng của Swift, đặc biệt là những tính năng cần cấu hình dự án phức tạp hoặc tập tin storyboard, sẽ không thể sử dụng ở đây.

## Xem thêm
- [Swift.org - Bắt đầu](https://www.swift.org/getting-started/#using-the-repl)
- Giới thiệu về Playgrounds trong Xcode của Apple (https://developer.apple.com/videos/play/wwdc2014/408/)
- [Dự án LLVM](https://llvm.org/)
