---
title:                "Làm việc với số phức"
aliases: - /vi/rust/working-with-complex-numbers.md
date:                  2024-01-28T22:12:41.210599-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
