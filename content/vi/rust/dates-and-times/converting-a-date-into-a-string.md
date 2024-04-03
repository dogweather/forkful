---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:45.939818-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y sang d\u1EA1ng chu\u1ED7\
  i trong Rust cho ph\xE9p hi\u1EC3n th\u1ECB ng\xE0y th\xE1ng d\u01B0\u1EDBi d\u1EA1\
  ng d\u1EC5 \u0111\u1ECDc cho con ng\u01B0\u1EDDi. Ch\xFAng ta l\xE0m \u0111i\u1EC1\
  u n\xE0y cho giao di\u1EC7n ng\u01B0\u1EDDi d\xF9ng\u2026"
lastmod: '2024-03-13T22:44:36.391710-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y sang d\u1EA1ng chu\u1ED7i trong\
  \ Rust cho ph\xE9p hi\u1EC3n th\u1ECB ng\xE0y th\xE1ng d\u01B0\u1EDBi d\u1EA1ng\
  \ d\u1EC5 \u0111\u1ECDc cho con ng\u01B0\u1EDDi."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Lý Do & Mục Đích

Chuyển đổi một ngày sang dạng chuỗi trong Rust cho phép hiển thị ngày tháng dưới dạng dễ đọc cho con người. Chúng ta làm điều này cho giao diện người dùng (UIs), nhật ký (logs), hoặc bất kỳ nơi nào mà mọi người cần hiểu rõ về các ngày tháng.

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
