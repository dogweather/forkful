---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:47.586509-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0\
  y th\xE1ng trong Rust, ch\xFAng ta s\u1EED d\u1EE5ng crate `chrono`, m\u1ED9t th\u01B0\
  \ vi\u1EC7n \u0111i \u0111\u1EBFn cho ng\xE0y v\xE0 gi\u1EDD. Tr\u01B0\u1EDBc ti\xEA\
  n, th\xEAm `chrono`\u2026"
lastmod: '2024-03-13T22:44:36.389167-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y th\xE1ng trong Rust,\
  \ ch\xFAng ta s\u1EED d\u1EE5ng crate `chrono`, m\u1ED9t th\u01B0 vi\u1EC7n \u0111\
  i \u0111\u1EBFn cho ng\xE0y v\xE0 gi\u1EDD."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Làm thế nào:
Để phân tích cú pháp ngày tháng trong Rust, chúng ta sử dụng crate `chrono`, một thư viện đi đến cho ngày và giờ.

Trước tiên, thêm `chrono` vào `Cargo.toml` của bạn:

```toml
[dependencies]
chrono = "0.4"
```

Sau đó, đây là một ví dụ đơn giản về việc phân tích cú pháp một ngày ISO 8601:

```rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let date_str = "2023-04-05";
    let parsed_date = date_str.parse::<NaiveDate>().unwrap();

    println!("Ngày được phân tích cú pháp là: {}", parsed_date);
}

```
Kết quả đầu ra:
```
Ngày được phân tích cú pháp là: 2023-04-05
```

## Sâu hơn
`chrono` là lựa chọn của Rust cho việc phân tích cú pháp ngày và giờ, gần như ngay từ khi Rust ra đời. Trước `chrono`, Rust có một thư viện thời gian cơ bản, nhưng nó thiếu tính năng. `chrono` đã lấp đầy khoảng trống đó.

Đối với các lựa chọn thay thế, bạn có crate `time`, nhưng `chrono` chiến thắng về độ phổ biến và bộ tính năng. Về mặt thực hiện, việc phân tích một chuỗi ngày tháng liên quan đến việc xác định định dạng và xử lý khả năng thất bại - đó là lý do tại sao chúng ta sử dụng `unwrap()`, điều này là ổn trong các ví dụ nhưng sử dụng `match` hoặc `unwrap_or_else` trong mã thực tế để xử lý lỗi một cách nhẹ nhàng.

Trong lịch sử, các ngôn ngữ lập trình đã gặp khó khăn với ngày và giờ. Đó là một vấn đề phức tạp do có năm nhuận, múi giờ và sự thay đổi giờ mùa hè. Đó là lý do tại sao các crate như `chrono` rất quý giá - chúng xử lý những điều lạ lùng cho chúng tôi.

## Xem Thêm
- Tài liệu chính thức của crate `chrono`: https://docs.rs/chrono/
- Hướng dẫn về xử lý lỗi của Rust API: https://rust-lang.github.io/api-guidelines/error.html
- Cái nhìn sâu sắc vào lịch sử thư viện thời gian của Rust: https://www.reddit.com/r/rust/comments/2z54zb/history_of_rusts_time_library/
