---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
aliases: - /vi/rust/removing-quotes-from-a-string.md
date:                  2024-01-28T22:06:35.067033-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc loại bỏ dấu ngoặc khỏi một chuỗi trong Rust liên quan đến việc loại bỏ các ký tự dấu ngoặc không cần thiết có thể được bao quanh dữ liệu văn bản của bạn. Lập trình viên thực hiện điều này khi họ cần làm sạch hoặc chuẩn hóa chuỗi, có thể sau khi phân tích dữ liệu từ một tệp, hoặc khi chuẩn bị cho một định dạng khác nơi mà dấu ngoặc có thể gây ra vấn đề hoặc thừa.

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
