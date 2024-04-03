---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:41.210599-07:00
description: "L\xE0m th\u1EBF n\xE0o: Rust kh\xF4ng c\xF3 h\u1ED7 tr\u1EE3 s\u1ED1\
  \ ph\u1EE9c s\u1EB5n c\xF3, nh\u01B0ng c\xE1c crate nh\u01B0 `num-complex` s\u1EBD\
  \ gi\xFAp b\u1EA1n. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch s\u1EED d\u1EE5ng n\xF3\
  ."
lastmod: '2024-03-13T22:44:36.367455-06:00'
model: gpt-4-0125-preview
summary: "Rust kh\xF4ng c\xF3 h\u1ED7 tr\u1EE3 s\u1ED1 ph\u1EE9c s\u1EB5n c\xF3, nh\u01B0\
  ng c\xE1c crate nh\u01B0 `num-complex` s\u1EBD gi\xFAp b\u1EA1n."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
Rust không có hỗ trợ số phức sẵn có, nhưng các crate như `num-complex` sẽ giúp bạn. Dưới đây là cách sử dụng nó:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Tổng: {}", sum); // Tổng: 3 - 1i
    println!("Tích: {}", product); // Tích: 14 - 5i
}
```
Bạn cần phải thêm `num_complex` vào `Cargo.toml` của mình để thực hiện điều kỳ diệu này.

## Sâu hơn
Số phức được hình thành vào thế kỷ 16 nhưng thực sự phát triển mạnh trong thế kỷ 18 khi các nhà toán học như Euler bắt đầu nghiên cứu chúng.

Không có các phép toán số phức nguyên bản, ngôn ngữ như Rust dựa vào thư viện bên thứ ba. `num-complex` là một crate như vậy và là một phần của bộ sưu tập crate `num` mà mục tiêu là cung cấp các kiểu và trait số cho Rust.

Đáng chú ý là một số ngôn ngữ (như Python) có hỗ trợ sẵn số phức, trong khi những ngôn ngữ khác (như C++, với tiêu đề `<complex>`) cung cấp chúng như một phần của thư viện chuẩn. Trong Rust, quyết định giữ thư viện chuẩn nhỏ gọn có nghĩa là bạn thường tìm đến các crate do cộng đồng tạo ra để có thêm chức năng.

## Xem thêm
- [Sách Rust](https://doc.rust-lang.org/book/): Để tìm hiểu thêm về Rust và cách làm việc với các crate bên ngoài.
- [Số Phức Wikipedia](https://en.wikipedia.org/wiki/Complex_number): Để hiểu sâu hơn về số phức.
