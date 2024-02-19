---
aliases:
- /vi/swift/rounding-numbers/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:00.685277-07:00
description: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u01B0\u1EDBc l\u01B0\u1EE3\
  ng m\u1ED9t gi\xE1 tr\u1ECB s\u1ED1 v\u1EDBi \u0111\u1ED9 ch\xEDnh x\xE1c nh\u1EA5\
  t \u0111\u1ECBnh, th\u01B0\u1EDDng nh\u1EB1m lo\u1EA1i b\u1ECF c\xE1c s\u1ED1 th\u1EAD\
  p ph\xE2n kh\xF4ng mong mu\u1ED1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
lastmod: 2024-02-18 23:08:51.084432
model: gpt-4-0125-preview
summary: "L\xE0m tr\xF2n s\u1ED1 c\xF3 ngh\u0129a l\xE0 \u01B0\u1EDBc l\u01B0\u1EE3\
  ng m\u1ED9t gi\xE1 tr\u1ECB s\u1ED1 v\u1EDBi \u0111\u1ED9 ch\xEDnh x\xE1c nh\u1EA5\
  t \u0111\u1ECBnh, th\u01B0\u1EDDng nh\u1EB1m lo\u1EA1i b\u1ECF c\xE1c s\u1ED1 th\u1EAD\
  p ph\xE2n kh\xF4ng mong mu\u1ED1n. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c\u2026"
title: "L\xE0m tr\xF2n s\u1ED1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm tròn số có nghĩa là ước lượng một giá trị số với độ chính xác nhất định, thường nhằm loại bỏ các số thập phân không mong muốn. Các lập trình viên thực hiện làm tròn để quản lý bộ nhớ, cải thiện khả năng đọc và đáp ứng các yêu cầu cụ thể của lĩnh vực như ràng buộc về tiền tệ.

## Làm thế nào:

Swift cung cấp một số cách để làm tròn số. Dưới đây là một số ví dụ:

```Swift
let original = 3.14159

// Làm tròn chuẩn
let standardRounded = round(original) // 3.0

// Làm tròn đến chỗ thập phân cụ thể
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Làm tròn xuống
let roundedDown = floor(original) // 3.0

// Làm tròn lên
let roundedUp = ceil(original) // 4.0

print("Chuẩn: \(standardRounded), Thập phân: \(decimalRounded), Xuống: \(roundedDown), Lên: \(roundedUp)")
```

Kết quả: `Chuẩn: 3.0, Thập phân: 3.142, Xuống: 3.0, Lên: 4.0`

## Sâu hơn

Về mặt lịch sử, làm tròn là một khái niệm toán học tồn tại trước cả máy tính, rất quan trọng trong thương mại và khoa học. Khung `Foundation` của Swift cung cấp chức năng làm tròn toàn diện:

- `round(_: )` là phương pháp làm tròn nửa lên truyền thống.
- `floor(_: )` và `ceil(_: )` xử lý việc làm tròn theo hướng.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` cung cấp sự kiểm soát tinh tế hơn với liệt kê quy tắc làm tròn.

Lưu ý về kiểu `Decimal` cho các tính toán tài chính chính xác, tránh lỗi số dấu phẩy động. Cũng khám phá `NSDecimalNumber` cho khả năng tương thích với Objective-C.

## Xem thêm

- Tiêu chuẩn IEEE cho Số học dấu phẩy động (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
