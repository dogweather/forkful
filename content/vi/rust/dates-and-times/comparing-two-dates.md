---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:15.085956-07:00
description: "So s\xE1nh hai ng\xE0y th\xE1ng c\xF3 ngh\u0129a l\xE0 ki\u1EC3m tra\
  \ xem ch\xFAng c\xF3 b\u1EB1ng nhau kh\xF4ng, ho\u1EB7c m\u1ED9t ng\xE0y n\xE0o\
  \ \u0111\xF3 di\u1EC5n ra tr\u01B0\u1EDBc ho\u1EB7c sau ng\xE0y kia. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.392934-06:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y th\xE1ng c\xF3 ngh\u0129a l\xE0 ki\u1EC3m tra xem\
  \ ch\xFAng c\xF3 b\u1EB1ng nhau kh\xF4ng, ho\u1EB7c m\u1ED9t ng\xE0y n\xE0o \u0111\
  \xF3 di\u1EC5n ra tr\u01B0\u1EDBc ho\u1EB7c sau ng\xE0y kia."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cái gì & Tại sao?
So sánh hai ngày tháng có nghĩa là kiểm tra xem chúng có bằng nhau không, hoặc một ngày nào đó diễn ra trước hoặc sau ngày kia. Lập trình viên sử dụng điều này để sắp xếp các sự kiện, xác thực đầu vào, xử lý hết hạn, và theo dõi thời lượng.

## Làm thế nào:
Rust sử dụng `chrono` để dễ dàng xử lý ngày tháng. Đầu tiên, `cargo.toml` cần `chrono = "0.4"`. Sau đó, bạn có thể so sánh ngày như sau:

```Rust
extern crate chrono;
use chrono::{DateTime, Utc};

fn main() {
    let date1: DateTime<Utc> = Utc::now();
    let date2: DateTime<Utc> = Utc::now(); // Thay đổi điều này để có kết quả khác nhau

    if date1 > date2 {
        println!("Ngày1 muộn hơn Ngày2");
    } else if date1 < date2 {
        println!("Ngày1 sớm hơn Ngày2");
    } else {
        println!("Ngày1 bằng với Ngày2");
    }
}
```

Kết quả mẫu khi `ngày1` muộn hơn:

```
Ngày1 muộn hơn Ngày2
```

## Đào Sâu
Ngày trước, trong những ngày đầu của Rust (thập kỷ 2010), việc so sánh ngày tháng khó khăn hơn - không có crate `chrono`. `chrono` đến và đơn giản hóa mọi thứ với các loại như `DateTime`. Trước `chrono`, chúng ta phải xử lý thời gian thủ công, dễ mắc lỗi.

Tại sao `chrono`? Nó trừu tượng hóa những phức tạp như múi giờ và năm nhuận, làm cho việc so sánh ngày tháng đáng tin cậy. Không có nó, bạn sẽ phải xử lý dấu thời gian Unix, cồng kềnh và kém dễ đọc.

Có những sự thay thế cho `chrono`, như crate `time`, nhưng `chrono` được sử dụng rộng rãi vì sự đơn giản và các tính năng của nó.

## Xem Thêm
- Tài liệu crate `chrono`: [docs.rs/chrono](https://docs.rs/chrono/)
- Tài liệu về khái niệm ngày và giờ chính thức của Rust: [doc.rust-lang.org/std/time](https://doc.rust-lang.org/std/time/index.html)
- So sánh crate `chrono` và `time`: [users.rust-lang.org](https://users.rust-lang.org/t/chrono-vs-time/45575)
