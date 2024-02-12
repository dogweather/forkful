---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:01:41.909350-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì và Tại sao?

Việc tạo ra các số ngẫu nhiên trong Rust liên quan đến việc sử dụng các thư viện để tạo ra các giá trị số không dự đoán trước, điều này là không thể thiếu cho các nhiệm vụ từ mã hoá và mô phỏng đến trò chơi và thuật toán ngẫu nhiên.

## Làm thế nào:

Rust dựa vào các crates bên ngoài để tạo số ngẫu nhiên, với `rand` là cái được sử dụng phổ biến nhất. Để bắt đầu tạo số ngẫu nhiên, bạn sẽ cần thêm `rand` vào tệp `Cargo.toml` của mình trước tiên:

```toml
[dependencies]
rand = "0.8.5"
```

Tiếp theo, bạn có thể tạo ra các số ngẫu nhiên bằng cách sử dụng `rand` trong mã Rust của mình. Dưới đây là một ví dụ về việc tạo ra một số nguyên ngẫu nhiên và một số thực dẫn:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Tạo một số nguyên ngẫu nhiên từ 1 đến 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Số Nguyên Ngẫu Nhiên: {}", random_int);
    
    // Tạo một số thực ngẫu nhiên từ 0.0 đến 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Số Thực Ngẫu Nhiên: {}", random_float);
}
```

Đầu ra mẫu có thể là:

```plaintext
Số Nguyên Ngẫu Nhiên: 7
Số Thực Ngẫu Nhiên: 0.9401077112175732
```

Lưu ý rằng chạy lại chương trình sẽ tạo ra các giá trị khác nhau.

## Tìm hiểu sâu

Việc tạo số ngẫu nhiên trong Rust, được hỗ trợ thông qua `rand` và các phụ thuộc của nó như `getrandom`, đại diện cho một trừu tượng rộng lớn qua các tiện ích hệ thống và các bộ tạo số thuật toán. Lịch sử, sự ngẫu nhiên trong máy tính đã tiến hóa từ các thuật toán đơn giản, dễ dự đoán đến các phương pháp an toàn mã hoá phức tạp. Cách tiếp cận của Rust bao hàm sự tiến hóa này qua tính năng mở rộng của nó `Rng`, có thể được hỗ trợ bởi các bộ tạo số khác nhau tùy thuộc vào chất lượng và hiệu suất ngẫu nhiên cần thiết.

Đối với hầu hết các ứng dụng, dựa vào `rand` và RNG của hệ thống cung cấp một sự cân bằng tốt giữa sự đơn giản và entropy. Tuy nhiên, đối với các ứng dụng mã hoá, các crates như `rand` chuyển sang sử dụng `getrandom` để seeding, chính nó dựa vào các cơ chế đặc biệt của hệ điều hành (ví dụ, `/dev/urandom` trên các hệ thống giống Unix), đảm bảo ngẫu nhiên an toàn mã hoá.

Ngoài ra, nếu bạn có những nhu cầu cụ thể không được `rand` đáp ứng, việc khám phá các crates khác hoặc triển khai các bộ tạo số tùy chỉnh dựa trên các mô hình toán học có thể là một con đường. Tuy nhiên, đối với đại đa số các trường hợp sử dụng, `rand` và hệ sinh thái của nó cung cấp các giải pháp mạnh mẽ vừa hiệu quả và dễ dàng tích hợp vào các ứng dụng Rust.
