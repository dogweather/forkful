---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:38.093395-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi ngh\u0129a l\xE0 t\u1EA1\
  o m\u1ED9t t\u1EC7p t\u1ED3n t\u1EA1i ng\u1EAFn h\u1EA1n cho vi\u1EC7c x\u1EED l\xFD\
  \ trung gian. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u m\xE0 kh\xF4ng l\xE0m\u2026"
lastmod: '2024-03-13T22:44:36.401926-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi ngh\u0129a l\xE0 t\u1EA1o\
  \ m\u1ED9t t\u1EC7p t\u1ED3n t\u1EA1i ng\u1EAFn h\u1EA1n cho vi\u1EC7c x\u1EED l\xFD\
  \ trung gian."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Gì và Tại sao?

Tạo một tệp tạm thời nghĩa là tạo một tệp tồn tại ngắn hạn cho việc xử lý trung gian. Lập trình viên thực hiện việc này để lưu trữ dữ liệu mà không làm lộn xộn không gian tệp của người dùng và đảm bảo thông tin nhạy cảm được xóa sau khi sử dụng.

## Làm thế nào:

Trong Rust, `tempfile` crate là một người bạn tốt cho những trò chơi tệp tạm. Thêm nó vào `Cargo.toml` của bạn:

```toml
[dependencies]
tempfile = "3.3.0"
```

Sau đó, bạn có thể tạo một tệp tạm như sau:

```rust
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    write!(temp_file, "Xin chào, thế giới!")?;

    let mut nội_dung = String::new();
    temp_file.reopen()?.read_to_string(&mut nội_dung)?;
    println!("Tệp tạm chứa: {}", nội_dung);

    // Tệp tạm sẽ được xóa ở đây khi `temp_file` ra khỏi phạm vi
    Ok(())
}
```

Chạy mã. Phép màu xảy ra. Một tệp xuất hiện, sau đó poof—biến mất khi bạn đã xong.

## Sâu hơn nữa

Trong lịch sử, tệp tạm thời cũ kỹ như các ngọn đồi trong lĩnh vực tính toán. Chúng luôn là một cách đơn giản nhưng hiệu quả để xử lý dữ liệu không cần lưu trữ dài hạn. Trong thế giới Rust, `tempfile` crate làm mượt mà quá trình tệp tạm, tự động dọn dẹp các tệp khi chúng không còn cần thiết, tránh được nỗi đau đầu cũ của việc dọn dẹp thủ công.

Có sự thay thế? Chắc chắn, bạn có thể tự tạo giải pháp của mình với `std::fs` và dọn dẹp thủ công, nhưng tại sao phải tái sáng tạo bánh xe?

Còn chi tiết? `tempfile` tạo các tệp trong thư mục tạm thời được chỉ định của hệ điều hành, và tên tệp được xáo trộn để ngăn chặn sự va chạm và tăng cường bảo mật.

## Xem thêm

- Tài liệu Rust `tempfile`: [https://docs.rs/tempfile/](https://docs.rs/tempfile/)
- Thư viện I/O chuẩn của Rust: [https://doc.rust-lang.org/std/io/](https://doc.rust-lang.org/std/io/)
- Khái niệm tệp tạm thời chung: [https://en.wikipedia.org/wiki/Temporary_file](https://en.wikipedia.org/wiki/Temporary_file)
