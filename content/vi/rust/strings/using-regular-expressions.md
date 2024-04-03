---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:15.354435-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust s\u1EED d\u1EE5ng crate `regex` cho c\xE1\
  c thao t\xE1c regex. \u0110\u1EA7u ti\xEAn, th\xEAm n\xF3 v\xE0o `Cargo.toml` c\u1EE7\
  a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.362269-06:00'
model: gpt-4-0125-preview
summary: "Rust s\u1EED d\u1EE5ng crate `regex` cho c\xE1c thao t\xE1c regex."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Làm thế nào:
Rust sử dụng crate `regex` cho các thao tác regex. Đầu tiên, thêm nó vào `Cargo.toml` của bạn:

```toml
[dependencies]
regex = "1"
```

Sau đó, bạn có thể khớp chuỗi như sau:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-01";

    println!("Liệu văn bản có khớp với mẫu ngày tháng? {}", re.is_match(date));
}
```

Đầu ra:

```
Liệu văn bản có khớp với mẫu ngày tháng? true
```

Đối với việc bắt các nhóm:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.(\w+)").unwrap();
    let email = "user@example.com";

    match re.captures(email) {
        Some(caps) => {
            println!("Người dùng: {}, Tên miền: {}, Phần mở rộng: {}", &caps[1], &caps[2], &caps[3]);
        }
        None => println!("Không tìm thấy kết quả."),
    }
}
```

Đầu ra:

```
Người dùng: user, Tên miền: example, Phần mở rộng: com
```

## Nghiên Cứu Sâu
Regex đã tồn tại từ những năm 1950, với nguồn gốc từ lý thuyết tự động và ngôn ngữ hình thức. Module `regex` của Rust được xây dựng để tăng tốc độ và an toàn, tập trung vào việc biên dịch các mẫu regex hiệu quả tại thời gian chạy. Các phương án thay thế cho regex bao gồm các hàm chuỗi như `find`, `split` và `replace`, phủ sóng các trường hợp sử dụng đơn giản hơn mà không cần mẫu. Regex trong Rust đặc biệt hiệu quả do được tối ưu hóa và biên dịch mẫu regex rộng rãi.

## Xem Thêm
- Tài liệu crate `regex`: https://docs.rs/regex/
- Phần về regex trong sách Rust: https://doc.rust-lang.org/book/ch18-00-patterns.html
- Chương Biểu Thức Chính Quy của "Ngôn Ngữ Lập Trình Rust": https://doc.rust-lang.org/stable/book/ch18-03-pattern-syntax.html
