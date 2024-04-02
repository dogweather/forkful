---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:53.686331-07:00
description: "So s\xE1nh hai ng\xE0y gi\u1ED1ng nh\u01B0 h\u1ECFi, \"Con g\xE0 hay\
  \ qu\u1EA3 tr\u1EE9ng c\xE1i n\xE0o c\xF3 tr\u01B0\u1EDBc?\" nh\u01B0ng v\u1EDB\
  i ng\xE0y tr\xEAn l\u1ECBch. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u \u0111\
  \xF3 \u0111\u1EC3 s\u1EAFp x\u1EBFp s\u1EF1 ki\u1EC7n, k\xEDch\u2026"
lastmod: '2024-03-13T22:44:37.114187-06:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y gi\u1ED1ng nh\u01B0 h\u1ECFi, \"Con g\xE0 hay qu\u1EA3\
  \ tr\u1EE9ng c\xE1i n\xE0o c\xF3 tr\u01B0\u1EDBc?\" nh\u01B0ng v\u1EDBi ng\xE0y\
  \ tr\xEAn l\u1ECBch. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u \u0111\xF3\
  \ \u0111\u1EC3 s\u1EAFp x\u1EBFp s\u1EF1 ki\u1EC7n, k\xEDch\u2026"
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cái gì & Tại sao?
So sánh hai ngày giống như hỏi, "Con gà hay quả trứng cái nào có trước?" nhưng với ngày trên lịch. Lập trình viên làm điều đó để sắp xếp sự kiện, kích hoạt hành động và đánh giá thời gian.

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
