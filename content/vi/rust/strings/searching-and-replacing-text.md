---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:18.504972-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: K\u1EBFt qu\u1EA3 m\u1EABu."
lastmod: '2024-04-05T21:53:37.766706-06:00'
model: gpt-4-0125-preview
summary: ''
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

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
