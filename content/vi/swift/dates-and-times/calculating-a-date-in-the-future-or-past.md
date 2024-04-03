---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:48.485435-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift l\xE0m cho vi\u1EC7c t\xEDnh to\xE1n ng\xE0\
  y tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n v\u1EDBi `Calendar` v\xE0 `DateComponents`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 b\u1EA3n ch\u1EA5t."
lastmod: '2024-03-13T22:44:37.115441-06:00'
model: gpt-4-0125-preview
summary: "Swift l\xE0m cho vi\u1EC7c t\xEDnh to\xE1n ng\xE0y tr\u1EDF n\xEAn \u0111\
  \u01A1n gi\u1EA3n v\u1EDBi `Calendar` v\xE0 `DateComponents`."
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Làm thế nào:
Swift làm cho việc tính toán ngày trở nên đơn giản với `Calendar` và `DateComponents`. Dưới đây là bản chất:

```Swift
import Foundation

// Ngày hôm nay
let today = Date()

// Lấy lịch hiện tại của người dùng
let currentCalendar = Calendar.current

// Thêm 2 tuần vào hôm nay
if let twoWeeksLater = currentCalendar.date(byAdding: .weekOfYear, value: 2, to: today) {
    print("Hai tuần từ bây giờ: \(twoWeeksLater)")
}

// Trừ đi 30 ngày từ hôm nay
if let thirtyDaysBefore = currentCalendar.date(byAdding: .day, value: -30, to: today) {
    print("Ba mươi ngày trước: \(thirtyDaysBefore)")
}
```

Kết quả có thể như sau:
```
Hai tuần từ bây giờ: 2023-04-14 10:26:47 +0000
Ba mươi ngày trước: 2023-03-15 10:26:47 +0000
```
Nhớ rằng, kết quả thực tế sẽ thay đổi vì `Date()` cung cấp cho bạn ngày và giờ hiện tại.

## Đi sâu hơn
Trước khi có Swift, Objective-C và cú pháp cồng kềnh của nó thống trị. Swift's `Date`, `Calendar`, và `DateComponents` đơn giản hóa các hoạt động với ngày. Những đối tượng này tôn trọng các múi giờ, xử lý sự thay đổi giờ mùa và tính toán dựa trên cài đặt lịch của người dùng - những yếu tố mà việc quản lý chúng trong Objective-C là một công việc lớn.

Các lựa chọn thay thế bao gồm thư viện bên thứ ba như SwiftDate, có thể cung cấp nhiều tiện ích và chức năng hơn. Nhưng với hầu hết mọi người, các công cụ có sẵn trong Swift hoạt động chỉ là tốt.

Ngày là phức tạp. Chúng không chỉ là những con số để tăng hoặc giảm; chúng bao gồm lịch, đặc thù theo địa phương, và các múi giờ. Khung nền tảng Foundation của Apple giải quyết sự phức tạp này, đảm bảo rằng các tính toán ngày trong tương lai và quá khứ của bạn có ý nghĩa trên toàn thế giới.
