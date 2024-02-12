---
title:                "So sánh hai ngày"
aliases:
- /vi/swift/comparing-two-dates.md
date:                  2024-01-28T21:56:53.686331-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
