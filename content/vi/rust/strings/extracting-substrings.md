---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:19.747816-07:00
description: "L\xE0m Th\u1EBF N\xE0o: H\xE3y th\u1EED ngay v\u1EDBi Rust. H\xE3y t\u01B0\
  \u1EDFng t\u01B0\u1EE3ng b\u1EA1n c\xF3 m\u1ED9t chu\u1ED7i, v\xE0 b\u1EA1n mu\u1ED1\
  n m\u1ED9t ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a n\xF3. B\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng c\xFA ph\xE1p c\u1EAFt chu\u1ED7i\u2026"
lastmod: '2024-03-13T22:44:36.361010-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y th\u1EED ngay v\u1EDBi Rust."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm Thế Nào:
Hãy thử ngay với Rust. Hãy tưởng tượng bạn có một chuỗi, và bạn muốn một phần cụ thể của nó. Bạn có thể sử dụng cú pháp cắt chuỗi `&str[start..end]` nơi `start` là nơi bạn bắt đầu, và `end` là nơi bạn kết thúc.

```Rust
fn main() {
    let text = "The quick brown fox jumps over the lazy dog";
    let quick_brown = &text[4..15]; // Cắt từ chỉ mục thứ 4 đến thứ 14
    println!("{}", quick_brown); // Xuất ra: quick brown
}
```

Việc cắt chuỗi là gọn gàng, nhưng có thể dẫn đến lỗi nếu chỉ mục của bạn không đúng với ranh giới của ký tự. Để tránh điều này, Rust cung cấp các phương pháp như `get`:

```Rust
fn main() {
    let text = "The quick brown fox";
    match text.get(4..15) {
        Some(substring) => println!("{}", substring), // cắt chuỗi an toàn
        None => println!("Phần cắt vượt qua giới hạn."),
    }
}

// Xuất ra: quick brown
```

Đó là một cái nhìn nhanh về việc rút trích chuỗi con trong Rust. Xem nào, thật dễ dàng phải không!

## Đào Sâu
Việc cắt chuỗi trong những ngôn ngữ có chuỗi mã hoá UTF-8 như Rust cần sự khéo léo—một ký tự có thể nhiều hơn một byte! Trước Rust, trong những ngôn ngữ như C, việc xử lý chuỗi có thể là nỗi đau đầu chứa đầy lỗi lầm, khi bạn phải tự quản lý bộ nhớ.

Kiểu `str` của Rust là một chuỗi các byte UTF-8, luôn là UTF-8 hợp lệ. Việc rút trích chuỗi con một cách an toàn tôn trọng những ranh giới ký tự này.

Các phương án khác để cắt bao gồm sử dụng vòng lặp hoặc regex cho các mẫu phức tạp hơn, nhưng chúng đều gây ra chi phí phát sinh. Khi cắt, Rust kiểm tra chỉ mục byte phải tương thích với ranh giới của ký tự char khi thực thi, ngăn chặn sự cố có thể xảy ra từ những lát cắt không hợp lệ.

## Xem Thêm
- Rust Book về chuỗi: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust qua Ví dụ – Chuỗi: https://doc.rust-lang.org/rust-by-example/std/str.html
- Tài liệu Rust cho `str`: https://doc.rust-lang.org/std/primitive.str.html
