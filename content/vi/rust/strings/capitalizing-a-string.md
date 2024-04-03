---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:19.708845-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust kh\xF4ng bao g\u1ED3m m\u1ED9t ph\u01B0\
  \u01A1ng th\u1EE9c s\u1EB5n c\xF3 \u0111\u1EC3 vi\u1EBFt hoa t\u1EEBng t\u1EEB trong\
  \ m\u1ED9t chu\u1ED7i, nh\u01B0ng ch\xFAng ta c\xF3 th\u1EC3 d\u1EC5 d\xE0ng tri\u1EC3\
  n khai ph\u01B0\u01A1ng ph\xE1p c\u1EE7a ri\xEAng\u2026"
lastmod: '2024-03-13T22:44:36.353110-06:00'
model: gpt-4-0125-preview
summary: "Rust kh\xF4ng bao g\u1ED3m m\u1ED9t ph\u01B0\u01A1ng th\u1EE9c s\u1EB5n\
  \ c\xF3 \u0111\u1EC3 vi\u1EBFt hoa t\u1EEBng t\u1EEB trong m\u1ED9t chu\u1ED7i,\
  \ nh\u01B0ng ch\xFAng ta c\xF3 th\u1EC3 d\u1EC5 d\xE0ng tri\u1EC3n khai ph\u01B0\
  \u01A1ng ph\xE1p c\u1EE7a ri\xEAng m\xECnh b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng ph\u01B0\
  \u01A1ng th\u1EE9c `to_ascii_uppercase` cho c\xE1c k\xFD t\u1EF1 \u0111\u01A1n v\xE0\
  \ l\u1EB7p qua c\xE1c t\u1EEB."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
Rust không bao gồm một phương thức sẵn có để viết hoa từng từ trong một chuỗi, nhưng chúng ta có thể dễ dàng triển khai phương pháp của riêng mình bằng cách sử dụng phương thức `to_ascii_uppercase` cho các ký tự đơn và lặp qua các từ.

```Rust
fn capitalize_words(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    let sentence = "hello world";
    println!("{}", capitalize_words(sentence));
}
```

Kết quả mẫu:

```
Hello World
```

## Sâu hơn:
Theo truyền thống, Rust đã ưu tiên một thư viện chuẩn có kích thước tối thiểu, với nhiều hàm tiện ích được cung cấp bởi cộng đồng thông qua các crates. Đối với việc viết hoa chuỗi, bạn có thể sử dụng crate `heck` để chuyển đổi trường hợp nâng cao hơn, như CamelCase, snake_case và hơn thế nữa.

Việc viết hoa một chuỗi có thể gặp khó khăn với các ký tự unicode. Kiểu `char` của Rust là một giá trị vô hình Unicode, cho phép xử lý đúng đắn hầu hết các ký tự. Khi xử lý với chuẩn hoá Unicode đầy đủ, các thư viện nâng cao hơn, như `unicode-segmentation`, nên được xem xét cho các thao tác chú ý tới các cụm grapheme.

Về mặt thực hiện, hàm `capitalize_words` của chúng tôi không phải là hiệu suất cao bởi vì nó cấp phát một `String` mới cho mỗi từ. Trong các ứng dụng đòi hỏi hiệu suất cao, sẽ có lợi khi tối ưu hóa thao tác chuỗi để tránh cấp phát bộ nhớ quá mức.

## Xem thêm:
- Tài liệu Rust về 'char': https://doc.rust-lang.org/std/primitive.char.html
- Crate 'Heck' cho chuyển đổi trường hợp: https://crates.io/crates/heck
- 'Chuẩn Hoá Dạng Unicode' trong Rust: https://unicode-rs.github.io/unicode-normalization/unicode_normalization/index.html
- Sách Rust để tìm hiểu thêm về chuỗi: https://doc.rust-lang.org/book/ch08-02-strings.html
