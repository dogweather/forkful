---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:18.504972-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/rust/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm kiếm và thay thế văn bản là quá trình tìm kiếm chuỗi trong các chuỗi và thay thế chúng bằng một cái gì đó khác. Lập trình viên thực hiện điều này để chỉnh sửa dữ liệu, tái cấu trúc mã, hoặc tự động hóa việc xử lý văn bản.

## Cách thực hiện:

```Rust
fn main() {
    let text = "Xin chào!";
    let updated_text = text.replace("chào", "thế giới");
    println!("{}", updated_text); // In ra "Xin thế giới!"
}
```

Kết quả mẫu:
```
Xin thế giới!
```

## Đi sâu hơn
Việc tìm kiếm và thay thế văn bản đã tồn tại từ khi các trình soạn thảo văn bản đầu tiên xuất hiện. Các công cụ như sed trong Unix đã làm cho việc xử lý văn bản hàng loạt trở nên phổ biến.

Rust áp dụng một cách tiếp cận hiệu quả và an toàn. Phương thức `replace`, từ kiểu `str` của thư viện chuẩn, là dễ hiểu và được kiểm tra tại thời điểm biên dịch.

Các phương thức thay thế cho `replace` bao gồm regex cho những mẫu phức tạp hoặc lặp qua các ký tự để tùy chỉnh logic thay thế.

Về cơ bản, `replace` trong Rust tạo một `String` mới, lặp qua chuỗi gốc, tìm các trận đấu, và sau đó xây dựng chuỗi mới với những thay đổi. Nó xử lý tốt Unicode, điều này không hề đơn giản.

## Xem thêm
- Tài liệu của Rust về `replace`: https://doc.rust-lang.org/std/primitive.str.html#method.replace
- Công cụ Regex cho các trường hợp sử dụng phức tạp hơn: https://crates.io/crates/regex
- Sổ tay hướng dẫn của Sed cho tài liệu tham khảo lịch sử: https://www.gnu.org/software/sed/manual/sed.html
