---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:09.151181-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi b\u1EB1ng Rust\
  \ c\xF3 ngh\u0129a l\xE0 thi\u1EBFt l\u1EADp m\u1ED9t c\u1EA5u tr\xFAc c\u01A1 b\u1EA3\
  n \u0111\u1EC3 m\xE3 c\u1EE7a b\u1EA1n c\xF3 m\u1ED9t n\u01A1i \u0111\u1EC3 \"s\u1ED1\
  ng\". C\xE1c l\u1EADp tr\xECnh vi\xEAn kh\u1EDFi \u0111\u1EA7u c\xE1c d\u1EF1 \xE1\
  n m\u1EDBi \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.377032-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi b\u1EB1ng Rust c\xF3\
  \ ngh\u0129a l\xE0 thi\u1EBFt l\u1EADp m\u1ED9t c\u1EA5u tr\xFAc c\u01A1 b\u1EA3\
  n \u0111\u1EC3 m\xE3 c\u1EE7a b\u1EA1n c\xF3 m\u1ED9t n\u01A1i \u0111\u1EC3 \"s\u1ED1\
  ng\"."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Cách thức:
Để bắt đầu một dự án Rust mới, bạn cần Cargo—quản lý gói Rust. Cài đặt Rust và Cargo thông qua trình cài đặt chính thức, rustup.

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

Sau đó, chỉ với một lệnh đơn giản để tạo một dự án mới:

```sh
cargo new my_project
```

Lệnh này tạo ra một thư mục mới có tên 'my_project' với tất cả các tệp cần thiết:

- `Cargo.toml`: Tệp khai báo của dự án bạn với metadata và các phụ thuộc.
- `src`: Thư mục chứa các tệp nguồn của bạn.
- `main.rs`: Điểm nhập chính cho chương trình của bạn.

Dưới đây là cách đơn giản nhìn của tệp `main.rs` sau khi được tạo:

```rust
fn main() {
    println!("Hello, world!");
}
```

Để biên dịch và chạy dự án của bạn:

```sh
cd my_project
cargo run
```

Và như bằng phép màu, bạn sẽ thấy kết quả:

```
   Compiling my_project v0.1.0 (path/to/my_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/my_project`
Hello, world!
```

## Sâu hơn nữa
Rust đã có quản lý gói và hệ thống biên dịch của riêng mình là Cargo, ngay từ những ngày đầu. Được tạo ra vào khoảng năm 2013, đó là cách của Rust để quản lý dự án, phụ thuộc, và quá trình biên dịch.

Tại sao Cargo lại tuyệt vời cho việc bắt đầu các dự án mới?

- **Nhất quán**: Nó tạo ra một cấu trúc dự án tiêu chuẩn.
- **Phụ thuộc**: Nó quản lý các thư viện bên ngoài một cách dễ dàng.
- **Biên dịch**: Nó biên dịch mã của bạn, tận dụng các tính năng an toàn và hiệu suất của Rust.

Các ngôn ngữ khác sử dụng các công cụ khác nhau—Node.js có npm, Ruby có Bundler, và Python có Pip. Cargo là câu trả lời của Rust cho những câu hỏi này và có lẽ làm được nhiều hơn từ ngay trong hộp công cụ bằng cách bao gồm cả hệ thống biên dịch, mà những ngôn ngữ khác giao cho các công cụ riêng biệt như Grunt hoặc Webpack trong hệ sinh thái JavaScript.

Các lựa chọn khác để bắt đầu dự án bằng Rust? Có lẽ, bạn có thể tự tổ chức mọi thứ hoặc sử dụng các môi trường phát triển tích hợp (IDEs), nhưng tại sao phải tái phát minh bánh xe khi Cargo đã giúp bạn làm phần nặng nhọc?

## Xem thêm
- Sách Lập Trình Rust: https://doc.rust-lang.org/book/
- Hướng dẫn cài đặt Rust và Cargo: https://www.rust-lang.org/tools/install
- Tài liệu Cargo: https://doc.rust-lang.org/cargo/
