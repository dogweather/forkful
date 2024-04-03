---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:15.721104-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Rust, ch\xFAng ta s\u1EED d\u1EE5ng macro\
  \ `format!`."
lastmod: '2024-03-13T22:44:36.356942-06:00'
model: gpt-4-0125-preview
summary: "Trong Rust, ch\xFAng ta s\u1EED d\u1EE5ng macro `format!`."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Làm thế nào:
Trong Rust, chúng ta sử dụng macro `format!`:

```Rust
fn main() {
    let name = "Ferris";
    let greeting = format!("Chào bạn, {}!", name);
    println!("{}", greeting); // In ra "Chào bạn, Ferris!"
}
```
Macro `format!` hoạt động giống như `println!`, nhưng nó trả về chuỗi đã định dạng thay vì in nó ra.

## Tìm hiểu sâu hơn
Rust đã chọn các macro như `format!` cho nội suy chuỗi thay vì cú pháp trong ngôn ngữ. Tại sao? Macro mạnh mẽ và linh hoạt—mở rộng chức năng ngôn ngữ mà không cần cú pháp phức tạp.

Trong lịch sử, những ngôn ngữ như C đã sử dụng các hàm như `sprintf`, rườm rà và dễ gây lỗi. Macro `format!` của Rust an toàn hơn, ngăn chặn các lỗi thường gặp.

Có các lựa chọn khác, như nối chuỗi bằng `+` hoặc macro `format_args!` để tránh cấp phát bộ nhớ trên heap. Nhưng nếu xét về sự dễ dàng và rõ ràng, `format!` là số một.

Lưu ý về hiệu suất: `format!` cấp phát bộ nhớ. Đối với mã cần tính hiệu suất cao, xem xét các phương pháp khác, như viết trực tiếp vào một bộ đệm.

## Xem thêm
- Tài liệu Rust chính thức về `format!`: https://doc.rust-lang.org/std/macro.format.html
- Sự khác biệt giữa `format!` và `println!`: https://doc.rust-lang.org/book/ch01-02-hello-world.html
- Rust by Example về định dạng: https://doc.rust-lang.org/rust-by-example/hello/print/print_display.html
