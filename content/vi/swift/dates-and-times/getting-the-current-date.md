---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:53.576629-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Vi\u1EC7c l\u1EA5y ng\xE0y v\xE0 gi\u1EDD\
  \ hi\u1EC7n t\u1EA1i trong Swift kh\xE1 \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:37.111669-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\u1EA5y ng\xE0y v\xE0 gi\u1EDD hi\u1EC7n t\u1EA1i trong Swift\
  \ kh\xE1 \u0111\u01A1n gi\u1EA3n."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

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
