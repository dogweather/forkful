---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:58:56.237931-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Xóa các ký tự khớp với một mẫu trong chuỗi nghĩa là tìm và loại bỏ các chuỗi ký tự cụ thể. Các lập trình viên thực hiện điều này để dọn dẹp văn bản, phân tích dữ liệu, hoặc điều chỉnh thông điệp để phù hợp với một định dạng cụ thể.

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
