---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:35.067033-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.359447-06:00'
model: gpt-4-0125-preview
summary: .
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Cách thực hiện:
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Xin chào, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Đầu ra: Xin chào, Rustaceans!
}
```

Đôi khi bạn có một chuỗi với dấu ngoặc hỗn hợp, như thế này:

```Rust
fn main() {
    let mixed_quoted = "'Rust nói: \"Xin chào, Thế giới!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Đầu ra: Rust nói: "Xin chào, Thế giới!"
}
```

Ở đây, chỉ có dấu ngoặc đơn ngoài cùng được loại bỏ.

## Đi Sâu Hơn
Khi loại bỏ dấu ngoặc khỏi một chuỗi, bạn có thể tự hỏi tại sao nó không chỉ đơn giản là `.replace("\"", "")`. Ngay từ đầu, việc xử lý văn bản ít được chuẩn hóa hơn, và các hệ thống khác nhau có những cách khác nhau để lưu trữ và truyền dẫn văn bản, thường đi kèm với một số loại 'chuỗi thoát' cho các ký tự đặc biệt. Phương thức `trim_matches` của Rust linh hoạt hơn, cho phép bạn chỉ định nhiều ký tự để cắt bỏ, và liệu có cắt từ đầu (tiền tố), cuối (hậu tố), hay cả hai bên của chuỗi.

Có những lựa chọn thay thế, tất nhiên. Regex là công cụ mạnh mẽ cho việc thao tác với chuỗi, có khả năng khớp với các mẫu phức tạp, và sẽ là dư thừa chỉ để loại bỏ dấu ngoặc. Thư viện như `trim_in_place` có thể cung cấp tính năng cắt bỏ tại chỗ mà không gây ra chi phí tạo ra một đối tượng `String` mới, đây có thể là điều mong muốn cho các ứng dụng cần hiệu suất cao.

Về bản chất, `trim_matches` thực sự duyệt qua các ký tự của chuỗi từ cả hai đầu, kiểm tra so với mẫu được cung cấp cho đến khi tìm thấy một ký tự không khớp. Nó hiệu quả cho những gì nó làm, nhưng luôn nhận thức được rằng nó đang làm việc với các giá trị vô hình Unicode. Nếu chuỗi của bạn có thể chứa các ký tự Unicode nhiều byte, bạn không phải lo lắng về việc nó phá vỡ chúng.

## Xem Thêm
- Tài liệu Rust về thao tác với chuỗi: https://doc.rust-lang.org/book/ch08-02-strings.html
- Crate `regex` cho các mẫu phức tạp: https://crates.io/crates/regex
- Rust by Example cho các tình huống lập trình thực tế: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
