---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:45.011623-07:00
description: "**C\xE1ch th\u1EF1c hi\u1EC7n:** Rust, m\u1ED9t ng\xF4n ng\u1EEF h\u1EC7\
  \ th\u1ED1ng t\u1EADp trung v\xE0o an to\xE0n v\xE0 hi\u1EC7u su\u1EA5t, kh\xF4\
  ng \u0111\u01B0\u1EE3c trang b\u1ECB c\xE1c ch\u1EE9c n\u0103ng ng\xE0y v\xE0 gi\u1EDD\
  \ trong th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a\u2026"
lastmod: '2024-04-05T22:50:50.732437-06:00'
model: gpt-4-0125-preview
summary: "Thay v\xE0o \u0111\xF3, c\u1ED9ng \u0111\u1ED3ng x\xE2y d\u1EF1ng c\xE1\
  c crate\u2014thu\u1EADt ng\u1EEF c\u1EE7a Rust cho th\u01B0 vi\u1EC7n ho\u1EB7c\
  \ g\xF3i."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

## **Cách thực hiện:**
```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now.format("%Y-%m-%d %H:%M:%S"));
}
```

Đầu ra:
```
2023-04-05 14:20:35
```

## **Tìm hiểu sâu hơn**
Rust, một ngôn ngữ hệ thống tập trung vào an toàn và hiệu suất, không được trang bị các chức năng ngày và giờ trong thư viện chuẩn của mình. Thay vào đó, cộng đồng xây dựng các crate—thuật ngữ của Rust cho thư viện hoặc gói. Một điển hình là `chrono`.

`chrono` cung cấp các tính năng datetime phong phú. Hơn nữa, nó xử lý các múi giờ, không phải là điều đơn giản. Crate sử dụng dữ liệu múi giờ từ `IANA` (Cơ Quan Phân Số Internet) để chính xác biểu diễn ngày và giờ địa phương.

Các crate khác như `time` tồn tại nhưng có thể có giao diện hoặc tính năng khác nhau. Đối với nhu cầu nhẹ hơn, `time` có thể nhanh hơn và có ít phụ thuộc hơn.

Lấy thời gian địa phương liên quan đến việc gọi hệ thống tương tác với hệ điều hành. Độ chính xác và độ chặt chẽ có thể thay đổi và bị ảnh hưởng bởi hệ thống và cấu hình của nó.

Các chi tiết thực thi cũng đáng được nhìn nhận dưới góc độ triết lý thiết kế. Rust ưa chuộng sự rõ ràng. Vì vậy, khi bạn lấy thời gian hiện tại, bạn chọn rõ ràng giữa thời gian địa phương so với UTC, ý thức về múi giờ, v.v.—giảm thiểu bất ngờ và thúc đẩy sự có chủ ý trong mã.

## **Xem thêm:**
- Tài liệu crate `chrono` của Rust: https://docs.rs/chrono/
- Tài liệu crate `time` của Rust: https://docs.rs/time/
- Cơ sở dữ liệu múi giờ của `IANA`: https://www.iana.org/time-zones
