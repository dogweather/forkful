---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:11.005377-07:00
description: "L\xE0m c\xE1ch n\xE0o: Th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9n c\u1EE7\
  a Rust gi\xFAp vi\u1EC7c \u0111\u1ECDc t\u1EC7p tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3\
  n."
lastmod: '2024-03-13T22:44:36.399357-06:00'
model: gpt-4-0125-preview
summary: "Th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9n c\u1EE7a Rust gi\xFAp vi\u1EC7c \u0111\
  \u1ECDc t\u1EC7p tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Làm cách nào:
Thư viện tiêu chuẩn của Rust giúp việc đọc tệp trở nên đơn giản.

```Rust
use std::fs::File;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let mut file = File::open("example.txt")?;
    let mut nội dung = String::new();
    file.read_to_string(&mut nội dung)?;
    println!("Nội dung Tệp:\n{}", nội dung);
    Ok(())
}
```
Đoạn mã này mở "example.txt", đọc nó, và in nội dung ra.

Ổ mẫu:
```
Nội dung Tệp:
Chào, Rustaceans!
```

## Tìm hiểu kỹ hơn
Về mặt lịch sử, I/O tệp có thể phức tạp, nhưng Rust đã làm đơn giản hóa nó. Có các phương án thay thế cho `read_to_string`, như sử dụng `BufRead` để xử lý từng dòng một, hiệu quả hơn đối với các tệp lớn hơn. Bên dưới, việc đọc tệp của Rust tận dụng các cuộc gọi hệ thống cấp HĐH, lưu trữ dữ liệu để tăng hiệu suất.

Sau Rust 1.0, ngôn ngữ này nhấn mạnh an toàn trong các tương tác hệ thống – việc đọc một tệp không ngoại lệ. Kiểu `Result` đóng gói các lỗi tiềm năng, giúp Rust tránh khỏi những lỗi thường gặp như tệp không tồn tại hoặc vấn đề quyền truy cập mà không phải sử dụng panics.

## Xem thêm
Ngữ liệu bổ sung để tham khảo:
- Tài liệu của Rust về I/O tệp: [std::fs](https://doc.rust-lang.org/std/fs/)
- Chương về xử lý lỗi trong Sách: [Xử lý lỗi](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- Rust bằng ví dụ về I/O tệp: [I/O tệp](https://doc.rust-lang.org/rust-by-example/std_misc/file.html)
