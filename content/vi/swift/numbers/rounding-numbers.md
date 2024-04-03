---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:00.685277-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift cung c\u1EA5p m\u1ED9t s\u1ED1 c\xE1ch\
  \ \u0111\u1EC3 l\xE0m tr\xF2n s\u1ED1. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t s\u1ED1 v\xED d\u1EE5."
lastmod: '2024-03-13T22:44:37.089936-06:00'
model: gpt-4-0125-preview
summary: "Swift cung c\u1EA5p m\u1ED9t s\u1ED1 c\xE1ch \u0111\u1EC3 l\xE0m tr\xF2\
  n s\u1ED1."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

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
