---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:13:25.541577-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Việc viết vào lối ra lỗi chuẩn (stderr) giống như để lại một dấu vết của kỹ thuật số — những lỗi và cảnh báo không nên xuất hiện trong kết quả xuất chuẩn. Các lập trình viên dùng stderr để báo cáo sự cố mà không làm ảnh hưởng đến xuất chuẩn (stdout), giữ cho nhật ký sạch sẽ và giúp dễ dàng phát hiện lỗi hơn.

## Cách thực hiện:

Trong Gleam, chúng ta có thể viết vào stderr sử dụng mô-đun `io`. Hãy bắt đầu thôi:

```gleam
import gleam/io

pub fn main() {
  io.stderr("Ồ không! Đã xảy ra sự cố.\n") // Gửi một chuỗi vào stderr
}
```

Mẫu đầu ra khi mọi thứ đi sai:
```
Ồ không! Đã xảy ra sự cố.
```

## Sâu hơn:

- Truyền thống, việc phân loại xuất ra thành stdout và stderr đã giúp tách biệt kết quả chương trình thông thường khỏi thông điệp lỗi.
- Các phương án khác bao gồm viết lỗi vào một tệp hoặc sử dụng một thư viện logging, nhưng stderr trực tiếp và đã sẵn có ngay tại đầu ngón tay bạn.
- Mô-đun `io` của Gleam xử lý stderr, trực tiếp đề cập đến khả năng của Erlang VM.

## Xem thêm

- Sách Gleam để biết thêm về xử lý lỗi: [https://gleam.run/book](https://gleam.run/book)
- "Môi trường lập trình Unix" cho bối cảnh lịch sử về các khái niệm stdout và stderr của Unix.
