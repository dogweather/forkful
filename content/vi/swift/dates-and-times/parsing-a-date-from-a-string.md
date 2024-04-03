---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:40.793495-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p m\u1ED9t ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7\
  i c\xF3 ngh\u0129a l\xE0 chuy\u1EC3n \u0111\u1ED5i bi\u1EC3u di\u1EC5n v\u0103n\
  \ b\u1EA3n c\u1EE7a m\u1ED9t ng\xE0y (nh\u01B0 \"2023-04-01\") th\xE0nh m\u1ED9\
  t \u0111\u1ED1i t\u01B0\u1EE3ng Ng\xE0y. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:37.110388-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p m\u1ED9t ng\xE0y t\u1EEB m\u1ED9t chu\u1ED7\
  i c\xF3 ngh\u0129a l\xE0 chuy\u1EC3n \u0111\u1ED5i bi\u1EC3u di\u1EC5n v\u0103n\
  \ b\u1EA3n c\u1EE7a m\u1ED9t ng\xE0y (nh\u01B0 \"2023-04-01\") th\xE0nh m\u1ED9\
  t \u0111\u1ED1i t\u01B0\u1EE3ng Ng\xE0y."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Làm thế nào:
Swift khiến việc phân tích cú pháp ngày trở nên khá dễ dàng với `DateFormatter`. Dưới đây là một ví dụ nhanh:

```Swift
import Foundation

let dateString = "2023-04-01"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"

if let parsedDate = dateFormatter.date(from: dateString) {
    print("Ngày đã phân tích cú pháp: \(parsedDate)")
} else {
    print("Không thành công trong việc phân tích cú pháp ngày.")
}
```

Kết quả mẫu có thể trông như thế này, tùy thuộc vào múi giờ của bạn:

```
Ngày đã phân tích cú pháp: 2023-03-31 22:00:00 +0000
```

Nhớ, kết quả mặc định ở dạng UTC!

## Đào sâu hơn
Kể từ sớm như trong Objective-C, các nhà phát triển iOS đã có `NSDateFormatter`, và nó đã được chuyển sang Swift dưới dạng `DateFormatter`. Trong quá khứ, việc xử lý ngày tháng là một vấn đề lớn do sự biến đổi về định dạng và múi giờ. May mắn thay, `DateFormatter` trong Swift đã chuẩn hóa quy trình này.

Mặc dù `DateFormatter` là tốt cho các tình huống thông thường, những phương án thay thế như `ISO8601DateFormatter` tồn tại cho các định dạng ISO 8601, và bạn có thể thậm chí đào sâu vào API cấp thấp hơn của `Cocoa` với `CFDateFormatter` để có thêm sự kiểm soát.

Khi triển khai phân tích cú pháp ngày, luôn thiết lập `locale` thành `posix` (`en_US_POSIX`) để tránh hành vi không mong muốn do cài đặt của người dùng. Ngoài ra, hãy lưu ý về hiệu năng. Việc phân tích cú pháp ngày là tốn kém, vì vậy hãy tái sử dụng bộ định dạng của bạn hoặc xem xét sử dụng `DateComponents` cho các nhiệm vụ lặp lại.

## Xem thêm
- [NSDateFormatter - Nhà Phát Triển Apple](https://developer.apple.com/documentation/foundation/nsdateformatter)
