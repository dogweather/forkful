---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:45.939818-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Crate `chrono` trong Rust l\xE0 l\u1EF1\
  a ch\u1ECDn h\xE0ng \u0111\u1EA7u cho vi\u1EC7c x\u1EED l\xFD ng\xE0y v\xE0 gi\u1EDD\
  . H\xE3y ch\u1EAFc ch\u1EAFn n\xF3 \u0111\xE3 \u0111\u01B0\u1EE3c th\xEAm v\xE0\
  o `Cargo.toml` c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.391710-06:00'
model: gpt-4-0125-preview
summary: "Crate `chrono` trong Rust l\xE0 l\u1EF1a ch\u1ECDn h\xE0ng \u0111\u1EA7\
  u cho vi\u1EC7c x\u1EED l\xFD ng\xE0y v\xE0 gi\u1EDD."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Cách thực hiện:
Crate `chrono` trong Rust là lựa chọn hàng đầu cho việc xử lý ngày và giờ. Hãy chắc chắn nó đã được thêm vào `Cargo.toml` của bạn:

```toml
[dependencies]
chrono = "0.4"
```

Bây giờ, chúng ta hãy định dạng một ngày thành chuỗi.

```rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

fn main() {
    let date: DateTime<Utc> = Utc::now(); // Lấy ngày và giờ hiện tại theo UTC.
    let formatted_date = date.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", formatted_date); // In ra: 2023-03-15 14:30:45
}
```

## Tìm Hiểu Sâu Hơn
Trước `chrono`, thư viện chuẩn của Rust có một số chức năng về ngày và giờ, nhưng chúng rất căn bản. `chrono` được xây dựng trên nền tảng đó để cung cấp chức năng toàn diện. Một lựa chọn khác có thể là crate `time` mới của Rust, hướng tới một API an toàn và dễ sử dụng hơn.

Khi bạn chuyển một ngày thành chuỗi, bạn đang thực hiện việc tuần tự hóa – chuyển đổi dữ liệu thành một định dạng có thể được chia sẻ hoặc lưu trữ. Định dạng bạn chọn (`%Y-%m-%d %H:%M:%S` trong trường hợp của chúng ta) tùy thuộc vào bạn, và `chrono` hỗ trợ nhiều mẫu như thế.

Bên trong, các ngày thường được lưu trữ dưới dạng dấu thời gian – giây từ một điểm bắt đầu, như epoch Unix (1 tháng 1 năm 1970). Khi bạn định dạng một ngày, bạn tính toán hình thức dễ đọc từ con số này, xem xét các múi giờ và giây nhuận.

## Xem Thêm
- Tài liệu crate `chrono`: https://docs.rs/chrono/
- Tài liệu crate `time` của Rust: https://docs.rs/time/
- Cú pháp định dạng ngày: http://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
