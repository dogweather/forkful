---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
date:                  2024-01-28T21:55:36.094510-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tính toán một ngày trong tương lai hoặc quá khứ có nghĩa là tìm ra lịch sẽ hiển thị như thế nào sau hoặc trước một khoảng thời gian cụ thể. Các lập trình viên thực hiện việc này cho mọi thứ liên quan đến thời gian, như cài đặt nhắc nhở, ngày hết hạn, hoặc lập kế hoạch cho các sự kiện.

## Cách thực hiện:

Rust sử dụng crate `chrono` cho mọi nhu cầu về ngày giờ của bạn. Dưới đây là cách để thêm hoặc trừ một ngày:

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("Thời gian UTC hiện tại: {}", now);

    let hai_tuan = Duration::weeks(2);
    let ngay_tuong_lai = now + hai_tuan;
    println!("UTC trong hai tuần: {}", ngay_tuong_lai);

    let ba_muoi_ngay_truoc = Duration::days(-30);
    let ngay_qua_khu = now + ba_muoi_ngay_truoc;
    println!("UTC 30 ngày trước: {}", ngay_qua_khu);
}
```

Kết quả mẫu:

```
Thời gian UTC hiện tại: 2023-04-01T12:00:00Z
UTC trong hai tuần: 2023-04-15T12:00:00Z
UTC 30 ngày trước: 2023-03-02T12:00:00Z
```

## Tìm hiểu Sâu

Truyền thống, việc thao tác với ngày và giờ đã là một vấn đề khó khăn. Các hệ thống và ngôn ngữ lập trình khác nhau xử lý nó theo các cách khác nhau. Thư viện chuẩn của Rust cung cấp chức năng cơ bản, nhưng crate `chrono` là sự lựa chọn hàng đầu.

Có phương án khác không? Chắc chắn rồi, bạn có thể tự tính toán các ngày bằng cách chuyển đổi tất cả thành dấu thời gian, thao tác với các con số, và chuyển ngược lại. Hoặc, bạn cũng có thể sử dụng các thư viện chuyên về thời gian trong các ngôn ngữ khác—Python có `datetime`, JavaScript có `Date`, v.v.

Crate `chrono` trong Rust cung cấp cho bạn các loại có ý thức về múi giờ như `DateTime`, và các khoảng thời gian như đã thấy ở trên. Nó xử lý tất cả các phần phức tạp như năm nhuận và giờ mùa hè để bạn không phải làm. Nó cũng thực hiện việc phân tích và định dạng ngày, làm cho nó trở thành một giải pháp toàn diện.

## Xem thêm

- Crate `chrono`: https://crates.io/crates/chrono
- Tài liệu về thời gian của Rust: https://doc.rust-lang.org/std/time/index.html
- Chương Ngày và Giờ trong Cuốn sách "Ngôn Ngữ Lập Trình Rust": https://doc.rust-lang.org/book/ch10-02-traits.html (tìm các phần liên quan đến DateTime)
