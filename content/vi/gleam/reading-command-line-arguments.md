---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:18.828154-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Đọc các đối số dòng lệnh cho phép các chương trình tác động lên dữ liệu được truyền vào khi khởi chạy. Lập trình viên sử dụng nó để tùy chỉnh hành vi mà không cần thay đổi code.

## Cách thực hiện:

Hàm `main` của Gleam có thể truy cập vào các đối số dòng lệnh thông qua một danh sách các chuỗi. Lặp lại, khớp mẫu và thực hiện tùy ý.

```gleam
import gleam/io

fn main(args: List(String)) {
  let message = match args {
    [] -> 
      "Không tìm thấy đối số nào."
    [single] -> 
      single
    _ -> 
      "Quá nhiều đối số!"
  }
  io.println(message)
}
```

Chạy chương trình:

```
$ my_program
Không tìm thấy đối số nào.

$ my_program "Hello, Gleam!"
Hello, Gleam!

$ my_program Quá nhiều đối số được cung cấp
Quá nhiều đối số!
```

## Nghiên cứu sâu

Đọc các đối số dòng lệnh đã là một phần không thể thiếu trong lập trình từ những ngày đầu. Các tiện ích UNIX xuất sắc trong việc này. Gleam, mặc dù có nguồn gốc từ Erlang VM, mang lại một hơi hướng hiện đại cho chức năng này. Các phương án thay thế bao gồm các thư viện phân tích cú pháp cho các trường hợp phức tạp, như các cờ tùy chọn. Gleam làm điều này mà không cần đến sự dài dòng của Erlang hay sự khó hiểu của C.

## Xem thêm

Để khám phá thêm:

- Tài liệu chính thức của Gleam: https://gleam.run/book
- `escript` của Erlang: http://erlang.org/doc/man/escript.html
- Các thư viện phân tích dòng lệnh: Xem kho lưu trữ gói của Gleam tại https://hex.pm/packages?search=gleam
