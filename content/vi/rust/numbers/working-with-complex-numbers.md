---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:41.210599-07:00
description: "S\u1ED1 ph\u1EE9c c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9t\
  \ ph\u1EA7n \u1EA3o, \u0111\xF3ng vai tr\xF2 quan tr\u1ECDng trong nhi\u1EC1u l\u0129\
  nh v\u1EF1c nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD v\xE0 \u0111\u1ED3 h\u1ECD\
  a m\xE1y t\xEDnh. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.367455-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9t ph\u1EA7\
  n \u1EA3o, \u0111\xF3ng vai tr\xF2 quan tr\u1ECDng trong nhi\u1EC1u l\u0129nh v\u1EF1\
  c nh\u01B0 k\u1EF9 thu\u1EADt, v\u1EADt l\xFD v\xE0 \u0111\u1ED3 h\u1ECDa m\xE1\
  y t\xEDnh. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Cái gì & Tại sao?
Số phức có một phần thực và một phần ảo, đóng vai trò quan trọng trong nhiều lĩnh vực như kỹ thuật, vật lý và đồ họa máy tính. Lập trình viên sử dụng chúng để giải các phương trình mà số thực thông thường không xử lý được.

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
