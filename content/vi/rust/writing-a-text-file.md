---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:12:49.606141-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?

Viết vào tệp văn bản là lưu dữ liệu dưới dạng ký tự đọc được trong một tệp trên thiết bị lưu trữ của bạn. Các lập trình viên thực hiện việc này để duy trì dữ liệu như cấu hình, log, hoặc nội dung do người dùng tạo.

## Làm thế nào:

Trong Rust, bạn sử dụng các module `std::fs::File` và `std::io::Write` để viết vào tệp.

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut file = File::create("output.txt").expect("Không thể tạo tệp");
    file.write_all(b"Hello, file!").expect("Không thể viết vào tệp");
}
```

Sau khi chạy này, bạn sẽ tìm thấy `output.txt` với `Hello, file!` là nội dung của nó.

## Thảo luận sâu hơn

Lịch sử, I/O tệp đã là một phần cơ bản của lập trình, trở lại từ thẻ đục và băng từ. Trong Rust, như trong nhiều ngôn ngữ lập trình hệ thống, viết vào tệp là một nhiệm vụ cơ bản nhưng cần phải xử lý lỗi một cách túc trí để đảm bảo tính ổn định.

Các phương pháp thay thế cho cách tiếp cận `std::fs::File` bao gồm các thư viện như `std::io::BufWriter` cho việc viết đệm hoặc các crate bên ngoài như `serde` để tuần tự hóa cấu trúc dữ liệu.

Các chi tiết triển khai bao gồm việc xử lý các kiểu `Result` trả về từ các thao tác I/O, đảm bảo lỗi được bắt và nguồn lực được quản lý một cách thích hợp — Hệ thống sở hữu của Rust đóng một vai trò chính trong việc quản lý các bộ điều khiển tệp và bộ đệm.

## Xem thêm

- Tài liệu chính thức của Rust về I/O tệp: https://doc.rust-lang.org/std/fs/
- Tìm hiểu về xử lý lỗi trong Rust: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- Để hiểu biết sâu hơn về I/O tệp, tìm hiểu `BufWriter`: https://doc.rust-lang.org/std/io/struct.BufWriter.html
- Khám phá `serde` cho tuần tự hóa dữ liệu: https://serde.rs/
