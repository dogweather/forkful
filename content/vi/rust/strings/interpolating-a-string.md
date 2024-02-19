---
aliases:
- /vi/rust/interpolating-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:15.721104-07:00
description: "N\u1ED9i suy chu\u1ED7i (String interpolation) ch\xE8n c\xE1c bi\u1EBF\
  n tr\u1EF1c ti\u1EBFp v\xE0o trong chu\u1ED7i. N\xF3 l\xE0m cho vi\u1EC7c x\xE2\
  y d\u1EF1ng chu\u1ED7i tr\u1EDF n\xEAn m\u01B0\u1EE3t m\xE0 v\xE0 d\u1EC5 \u0111\
  \u1ECDc, tr\xE1nh \u0111\u01B0\u1EE3c vi\u1EC7c\u2026"
lastmod: 2024-02-18 23:08:50.446283
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i (String interpolation) ch\xE8n c\xE1c bi\u1EBFn\
  \ tr\u1EF1c ti\u1EBFp v\xE0o trong chu\u1ED7i. N\xF3 l\xE0m cho vi\u1EC7c x\xE2\
  y d\u1EF1ng chu\u1ED7i tr\u1EDF n\xEAn m\u01B0\u1EE3t m\xE0 v\xE0 d\u1EC5 \u0111\
  \u1ECDc, tr\xE1nh \u0111\u01B0\u1EE3c vi\u1EC7c\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nội suy chuỗi (String interpolation) chèn các biến trực tiếp vào trong chuỗi. Nó làm cho việc xây dựng chuỗi trở nên mượt mà và dễ đọc, tránh được việc kết nối cồng kềnh.

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
