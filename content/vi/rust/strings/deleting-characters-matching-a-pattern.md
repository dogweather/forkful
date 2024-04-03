---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:56.237931-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu trong\
  \ chu\u1ED7i ngh\u0129a l\xE0 t\xECm v\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i k\xFD\
  \ t\u1EF1 c\u1EE5 th\u1EC3. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7\
  n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 d\u1ECDn d\u1EB9p v\u0103n b\u1EA3n, ph\xE2\
  n\u2026"
lastmod: '2024-03-13T22:44:36.354453-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu trong chu\u1ED7\
  i ngh\u0129a l\xE0 t\xECm v\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i k\xFD t\u1EF1\
  \ c\u1EE5 th\u1EC3."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Làm thế nào:
Trong Rust, chúng ta có thể sử dụng phương thức `replace` từ kiểu `String` hoặc regex cho các mẫu phức tạp hơn. Dưới đây là cách bạn làm:

```rust
fn main() {
    let phrase = "Hello, _world_! -- Programming in Rust --".to_string();
    // Thay thế gạch dưới bằng không
    let cleaned = phrase.replace("_", "");
    println!("{}", cleaned);

    // Sử dụng regex cho các mẫu phức tạp hơn (nhớ thêm crate regex vào Cargo.toml)
    let regex = regex::Regex::new(r"--.*?--").unwrap();
    let s = regex.replace_all(&cleaned, "");
    println!("{}", s);
}

// Đầu ra:
// Hello, world! -- Programming in Rust --
// Hello, world!
```

## Sâu hơn nữa
Việc xóa các ký tự khớp với một mẫu không chỉ riêng gì Rust; đây là một hoạt động thông thường trong nhiều ngôn ngữ lập trình. Trong quá khứ, các công cụ như `sed` trong Unix đã được sử dụng để biến đổi văn bản theo những cách mạnh mẽ, và bây giờ các ngôn ngữ cung cấp các hàm tích hợp sẵn cho việc thao tác chuỗi.

Trong Rust, cách tiếp cận tiêu chuẩn là sử dụng `replace` cho các mẫu cố định đơn giản. Đối với các biểu tượng đại diện, lặp lại, hoặc loại bỏ có điều kiện, chúng ta sẽ chuyển sang regex. Crate regex là công cụ de facto cho việc này, nhưng nhớ rằng, các hoạt động regex tiêu tốn nhiều chi phí về hiệu suất, vì vậy hãy sử dụng chúng một cách thận trọng.

Các đảm bảo về sự an toàn của Rust cũng mở rộng đến việc xử lý văn bản. Trong khi ở một số ngôn ngữ, việc thao tác chuỗi có thể là nguồn gốc của các lỗ hổng bảo mật như tràn bộ đệm, thiết kế của Rust bảo vệ chống lại những vấn đề như vậy.

## Xem thêm
- Tài liệu Rust `String`: https://doc.rust-lang.org/std/string/struct.String.html 
- Tài liệu crate `regex`: https://docs.rs/regex/
- Sách Rust Regex: https://rust-lang-nursery.github.io/regex/
