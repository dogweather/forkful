---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:53.686331-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift s\u1EED d\u1EE5ng ki\u1EC3u `Date` cho\
  \ ng\xE0y v\xE0 gi\u1EDD. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u01A1\
  n gi\u1EA3n \u0111\u1EC3 so s\xE1nh hai ng\xE0y."
lastmod: '2024-03-13T22:44:37.114187-06:00'
model: gpt-4-0125-preview
summary: "Swift s\u1EED d\u1EE5ng ki\u1EC3u `Date` cho ng\xE0y v\xE0 gi\u1EDD."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Làm thế nào:
Swift sử dụng kiểu `Date` cho ngày và giờ. Dưới đây là cách đơn giản để so sánh hai ngày:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"

// Tạo hai đối tượng ngày
let date1 = dateFormatter.date(from: "2023/01/01 09:00")!
let date2 = dateFormatter.date(from: "2023/02/01 10:00")!

// So sánh ngày
if date1 == date2 {
    print("Hai ngày giống nhau")
} else if date1 < date2 {
    print("Date1 sớm hơn Date2")
} else {
    print("Date1 muộn hơn Date2")
}
```

Đầu ra mẫu:

`Date1 sớm hơn Date2`

Các toán tử so sánh có thể được sử dụng vì `Date` tuân thủ giao thức `Comparable`.

## Sâu hơn:
Ngày không phải lúc nào cũng dễ dàng như các đối tượng. Ban đầu, bạn phải xử lý từng thành phần riêng lẻ như năm, tháng và ngày. Tệ hơn nhiều. Bây giờ, các đối tượng `Date` trong Swift xử lý công việc nặng nhọc, và việc so sánh chúng trở nên đơn giản với các toán tử có sẵn.

Trước Swift và `Date` của Cocoa, Objective-C sử dụng `NSDate`, nhưng chúng có thể cầu nối, vì vậy mã cũ vẫn có thể hoạt động tốt.

Và này, không chỉ `<`, `>`, và `==` — bạn cũng có thể sử dụng `timeIntervalSince(_:)` để kiểm soát tỉ mỉ hơn, như:

```Swift
let timeInterval = date2.timeIntervalSince(date1)
```

Điều này cho bạn biết sự chênh lệch bằng giây. Giá trị dương: date2 trước; giá trị âm: nó sau; không: chúng giống hệt nhau. Cực kỳ hữu ích cho bộ hẹn giờ, đếm ngược và theo dõi thời gian. Bên dưới bề mặt, ngày chỉ là các điểm tham chiếu trong thời gian—hãy nghĩ về chúng như những dấu thời gian sang trọng.

## Xem thêm:
- Tài liệu về Date của Apple: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- Hướng dẫn Định dạng Ngày: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)
