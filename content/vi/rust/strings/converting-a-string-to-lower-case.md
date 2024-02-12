---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases: - /vi/rust/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:58:16.296907-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chuyển đổi một chuỗi thành chữ thường có nghĩa là biến mỗi chữ trong chuỗi thành chữ cái nhỏ. Điều này rất tiện lợi cho việc so sánh không phân biệt kiểu chữ hoặc chuẩn bị văn bản cho quá trình xử lý đồng nhất.

## Làm thế nào:
```Rust
fn main() {
    let greeting = "HeLLo, WoRlD!";
    let lowercase_greeting = greeting.to_lowercase();
    println!("{}", lowercase_greeting); // "hello, world!"
}
```
Đầu ra:
```
hello, world!
```

## Sâu hơn
Trước phương thức `.to_lowercase()`, bạn có thể đã thấy người dùng Rust sử dụng `.to_ascii_lowercase()` cho cùng một nhiệm vụ, nhưng nó chỉ ảnh hưởng đến các ký tự ASCII. Thư viện tiêu chuẩn Rust đã phát triển, cung cấp `.to_lowercase()` hỗ trợ Unicode đầy đủ—nghĩa là nó có thể xử lý nhiều hơn chỉ là tiếng Anh! Điều này rất quan trọng nếu ứng dụng của bạn bước ra ngoài thế giới đa ngôn ngữ.

Có gì bên dưới cấp? Có lẽ, phương thức `to_lowercase()` không chỉ đơn giản là thay thế 'A' thành 'a'. Nó giống như một nhà ngôn ngữ học tí hon, có kiến thức về cách thức của Unicode. Nó tuân theo tiêu chuẩn Unicode để chính xác chuyển đổi các ký tự thành chữ thường, tôn trọng những điều tế nhị văn hóa của chúng.

Dĩ nhiên, có những phương án thay thế. Bạn có thể bắt đầu một vòng lặp, đi qua từng ký tự và tự chuyển đổi chúng. Nhưng tại sao phải tái phát minh bánh xe khi thư viện tiêu chuẩn của Rust đã đầu tư công sức?

## Xem Thêm
- [Tài liệu Rust về `to_lowercase()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Tài liệu String của Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Bản đồ trường hợp Unicode](https://www.unicode.org/reports/tr21/tr21-5.html)
