---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:58.895154-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift s\u1EED d\u1EE5ng `DateFormatter` \u0111\
  \u1EC3 chuy\u1EC3n \u0111\u1ED5i \u0111\u1ED1i t\u01B0\u1EE3ng `Date` th\xE0nh chu\u1ED7\
  i d\u1EC5 \u0111\u1ECDc. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch th\u1EE9c."
lastmod: '2024-03-13T22:44:37.112914-06:00'
model: gpt-4-0125-preview
summary: "Swift s\u1EED d\u1EE5ng `DateFormatter` \u0111\u1EC3 chuy\u1EC3n \u0111\u1ED5\
  i \u0111\u1ED1i t\u01B0\u1EE3ng `Date` th\xE0nh chu\u1ED7i d\u1EC5 \u0111\u1ECD\
  c."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Làm thế nào:
Swift sử dụng `DateFormatter` để chuyển đổi đối tượng `Date` thành chuỗi dễ đọc. Dưới đây là cách thức:

```Swift
import Foundation

let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: date)
print(dateString) // Kết quả: "2023-04-05 14:20:35" (hoặc ngày và giờ hiện tại)
```

Thay đổi `dateFormat` để điều chỉnh cách hiển thị ngày tháng của bạn:

```Swift
formatter.dateFormat = "EEEE, MMM d, yyyy"
print(formatter.string(from: date)) // Kết quả: "Thứ Tư, Apr 5, 2023"
```

## Đào Sâu
Trước `DateFormatter`, Objective-C và phiên bản Swift đầu tiên sử dụng `NSDateFormatter`, về cơ bản là cùng một thứ được đổi tên. Điều quan trọng là biết đến ISO 8601, một tiêu chuẩn định dạng ngày tháng phổ biến. Các nhà phát triển phải cân bằng giữa các định dạng tùy chỉnh với cài đặt địa phương của người dùng. Tại sao? Bởi vì cách đọc ngày tháng khác nhau trên toàn thế giới. Ví dụ, người Mỹ sử dụng "MM/dd/yyyy", trong khi nhiều quốc gia Châu Âu sử dụng "dd/MM/yyyy".

Có phương thức thay thế không? Chắc chắn rồi. Swift cung cấp `ISO8601DateFormatter` cho các ngày tháng ISO 8601, và `DateComponentsFormatter` cho chuỗi thời lượng, như "42 phút". Bạn cũng có thể tự tạo tùy chỉnh với `.formatted()` trong Swift 5.5 trở lên:

```Swift
let formattedDate = date.formatted(.dateTime.year().month().day().hour().minute().second())
print(formattedDate) // Kết quả sẽ tùy thuộc vào cài đặt địa phương của bạn
```

Hãy cẩn trọng: Việc tạo chuỗi tùy chỉnh có thể dẫn đến nhức đầu về vấn đề địa phương hóa và code dễ xảy ra lỗi. Hãy gắn bó với các bộ định dạng và tiêu chuẩn khi có thể.

## Xem Thêm
- [Định dạng Ngày](https://developer.apple.com/documentation/foundation/dateformatter) - Tài liệu của Apple về DateFormatter
