---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-01-28T22:05:10.570232-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì & Tại sao?
In thông tin để gỡ lỗi (debug output) cho phép bạn xem qua trạng thái của chương trình mà không cần phải sử dụng một trình gỡ lỗi đầy đủ. Nó nhanh chóng, không cầu kỳ, và hoàn hảo để truy tìm những lỗi phức tạp khi bạn không cần sức mạnh của một công cụ gỡ lỗi chuyên dụng.

## Cách thực hiện:

Để in ra một cái gì đó đơn giản, sử dụng `println!`. Nếu bạn cần in một giá trị để gỡ lỗi, `dbg!` sẽ rất tiện lợi.

```Rust
fn main() {
    let mut vec = vec![1, 2, 3];
    
    // In cơ bản
    println!("Xin chào, các Rustaceans!");

    // Định dạng gỡ lỗi với println! sử dụng `{:?}`
    println!("{:?}", vec);

    // Gỡ lỗi với `dbg!`, in ra stderr và trả về giá trị
    dbg!(&vec);

    // Sửa đổi vec sau khi sử dụng `dbg!`
    vec.push(4);
    dbg!(vec);
}
```

Kết quả mẫu:

```
Xin chào, các Rustaceans!
[1, 2, 3]
[src/main.rs:9] &vec = [
    1,
    2,
    3,
]
[src/main.rs:13] vec = [
    1,
    2,
    3,
    4,
]
```

## Đi sâu vào vấn đề

Việc in thông tin gỡ lỗi là một phần đơn giản của lập trình từ những ngày đầu. Sự đơn giản của nó thường làm cho nó trở thành lựa chọn hàng đầu để chẩn đoán nhanh các vấn đề.

Trong Rust, `println!` tuyệt vời để hiển thị thông điệp thân thiện với người dùng. Điều thú vị nằm ở `dbg!`, được giới thiệu trong Rust 1.32, in cả giá trị và vị trí của nó trong mã. Đầu ra là lỗi chuẩn (stderr), vì vậy nó sẽ không trộn lẫn với đầu ra chuẩn (stdout) và có thể được chuyển hướng riêng biệt nếu cần.

Đối với các kiểu phức tạp, bạn có thể derive `Debug` trait để tự động tạo một định dạng mà `println!` và `dbg!` có thể sử dụng. Đó là những gì annotation `#[derive(Debug)]` làm trên các structs và enums của bạn.

Về các lựa chọn khác, có tồn tại các logger chính thống như `log` và `env_logger`, và nếu bạn cần kiểm soát một cách chi tiết hơn, xem xét sử dụng một trình gỡ lỗi như `gdb` hay `lldb`, hoạt động với Rust thông qua các tích hợp như `rust-gdb` hoặc `rust-lldb`.

## Tham khảo thêm

Để biết thêm về các tùy chọn in và định dạng gỡ lỗi của Rust:

- Sách Rust về `println!` và Định dạng: https://doc.rust-lang.org/std/fmt/index.html
- Tài liệu macro `dbg!`: https://doc.rust-lang.org/std/macro.dbg.html
- Hướng dẫn chính thức để gỡ lỗi với `gdb` và `lldb`: https://rust-lang.github.io/rustup-components-history
- Crate `log` cho một cách tiếp cận cấu trúc hơn đối với việc log: https://crates.io/crates/log
- Crate `env_logger`, một triển khai logger phổ biến cho facade `log`: https://crates.io/crates/env_logger
