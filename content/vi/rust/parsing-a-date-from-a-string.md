---
title:                "Phân tích ngày từ chuỗi kí tự"
date:                  2024-01-28T22:04:47.586509-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Phân tích cú pháp ngày tháng từ một chuỗi nghĩa là chuyển đổi văn bản thành định dạng ngày mà mã của bạn có thể hiểu. Chúng ta thực hiện việc này vì ngày tháng thường được truyền đến dưới dạng chuỗi từ đầu vào của người dùng hoặc nguồn dữ liệu bên ngoài và chúng ta cần chúng ở dạng cấu trúc để tính toán và lưu trữ.

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
