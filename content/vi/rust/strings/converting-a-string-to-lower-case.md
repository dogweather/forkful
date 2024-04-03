---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:16.296907-07:00
description: "L\xE0m th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:36.358184-06:00'
model: gpt-4-0125-preview
summary: .
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

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
