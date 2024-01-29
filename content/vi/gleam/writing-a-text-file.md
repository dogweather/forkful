---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:40.328681-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc viết một tệp văn bản có nghĩa là lưu dữ liệu dưới dạng văn bản mà con người có thể đọc được. Các lập trình viên thực hiện điều này để lưu đầu ra, cấu hình ứng dụng, hoặc ghi lại các sự kiện.

## Làm thế nào:
Gleam cung cấp tính năng nhập/xuất tệp qua thư viện chuẩn của mình. Dưới đây là cách viết vào một tệp:
```gleam
import gleam/io
import gleam/result

pub fn write_to_file(contents: String) -> Result(Nil, IOError) {
  result.then(
    io.open("output.txt", [io.Write]),
    fn(file) { io.write(file, contents) }
  )
}

pub fn main() {
  case write_to_file("Chào, Gleam!") {
    Ok(_) -> io.print("Viết vào tệp thành công")
    Error(err) -> io.print(err)
  }
}
```
Nếu thành công, `output.txt` của bạn sẽ chứa "Chào, Gleam!".

## Đi sâu hơn
Lịch sử, việc xử lý tệp là rất quan trọng cho việc lưu trữ dữ liệu lâu dài. Cách tiếp cận của Gleam tương tự như Erlang, nền tảng mà nó được xây dựng trên. Các phương án thay thế bao gồm hệ thống cơ sở dữ liệu hoặc lưu trữ tạm thời trong bộ nhớ cho dữ liệu tạm thời. Thư viện chuẩn của Gleam giữ một API gọn gàng, ưu tiên xử lý lỗi một cách rõ ràng với kiểu `Result`.

## Xem thêm
- [Mô-đun File của Erlang](http://erlang.org/doc/man/file.html) để hiểu về các hoạt động cấp thấp mà Gleam tinh giản.
- [Viết tệp trong Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.create), để so sánh với một ngôn ngữ hệ thống khác.
