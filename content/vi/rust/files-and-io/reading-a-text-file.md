---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:11.005377-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 vi\u1EC7c l\u1EA5\
  y n\u1ED9i dung v\u0103n b\u1EA3n t\u1EEB m\u1ED9t t\u1EC7p .txt tr\xEAn \u1ED5\
  \ \u0111\u0129a c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 x\u1EED l\xFD d\u1EEF li\u1EC7u nh\u01B0 c\u1EA5u h\xECnh,\
  \ nh\u1EADp li\u1EC7u\u2026"
lastmod: 2024-02-19 22:04:55.554228
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n l\xE0 vi\u1EC7c l\u1EA5\
  y n\u1ED9i dung v\u0103n b\u1EA3n t\u1EEB m\u1ED9t t\u1EC7p .txt tr\xEAn \u1ED5\
  \ \u0111\u0129a c\u1EE7a b\u1EA1n. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 x\u1EED l\xFD d\u1EEF li\u1EC7u nh\u01B0 c\u1EA5u h\xECnh,\
  \ nh\u1EADp li\u1EC7u\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Đọc một tệp văn bản là việc lấy nội dung văn bản từ một tệp .txt trên ổ đĩa của bạn. Lập trình viên làm điều này để xử lý dữ liệu như cấu hình, nhập liệu của người dùng, hoặc để xử lý văn bản số lượng lớn.

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
