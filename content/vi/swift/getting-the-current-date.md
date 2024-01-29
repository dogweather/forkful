---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:01:53.576629-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trong Swift, việc lấy ngày hiện tại đòi hỏi truy cập vào cài đặt thời gian và ngày của hệ thống. Lập trình viên thực hiện điều này để ghi thời gian của sự kiện, lập lịch nhiệm vụ hoặc đơn giản chỉ là hiển thị ngày và giờ trong ứng dụng của họ.

## Cách thực hiện:
Việc lấy ngày và giờ hiện tại trong Swift khá đơn giản:

```Swift
import Foundation

let currentDate = Date()
print(currentDate)
```
Kết quả mẫu:
```
2023-04-10 16:20:32 +0000
```
Nếu bạn muốn một định dạng cụ thể:

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let formattedDate = formatter.string(from: Date())
print(formattedDate)
```
Kết quả mẫu:
```
2023-04-10 16:20:32
```

## Tìm hiểu kỹ hơn
Struct `Date` trong Swift là một phần của framework Foundation, được mượn từ `NSDate` của Objective-C. Theo thời gian, Swift cung cấp một cách tiếp cận hiện đại hơn với `Date` mà thể hiện mạch lạc và an toàn hơn.

Có những phương pháp khác với `Date()` để lấy thời gian hiện tại. Ví dụ, `NSDate()`, khá giống nhưng không thân thiện với Swift như vậy, và các API mức thấp hơn như `gettimeofday()` để lấy thời gian hệ thống chính xác hơn. Nhưng, `Date()` là sự lựa chọn hàng đầu đối với hầu hết các nhà phát triển Swift bởi nó cân bằng giữa sự dễ sử dụng với độ chính xác đủ dùng cho hầu hết trường hợp.

`Date()` trong Swift lấy thời gian hệ thống, thường là theo Thời gian Phối hợp Quốc tế (UTC). Vì vậy, khi bạn in trực tiếp nó không có định dạng, nó xuất hiện với độ lệch UTC. Đó là lý do tại sao các định dạng rất phổ biến; chúng điều chỉnh ngày và giờ sang múi giờ và định dạng cụ thể nào đó, làm cho nó thân thiện với con người khi hiển thị. Việc thực hiện điều chỉnh múi giờ của bạn mà không cần đến định dạng là có thể nhưng tự sáng tạo lại bánh xe và dễ mắc lỗi do những thay đổi của giờ mùa và giây nhuận.

## Xem thêm
- Tài liệu chính thức về `Date` của Apple: [Date - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- Hướng dẫn về DateFormatter: [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- Để hiểu sâu hơn về ngày và giờ trong hệ thống máy tính, xem video của Computerphile trên YouTube](https://www.youtube.com/watch?v=-5wpm-gesOY).
