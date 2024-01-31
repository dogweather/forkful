---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:56:19.708845-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc viết hoa một chuỗi nghĩa là làm cho chữ cái đầu tiên của mỗi từ trở nên in hoa trong khi những chữ còn lại giữ nguyên dạng chữ thường. Lập trình viên thực hiện điều này vì mục đích định dạng, để tuân thủ các quy tắc ngôn ngữ trong giao diện người dùng, hoặc để đảm bảo sự nhất quán của dữ liệu trong xử lý văn bản.

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
