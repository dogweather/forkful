---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:53.576629-07:00
description: "Trong Swift, vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i \u0111\xF2\
  i h\u1ECFi truy c\u1EADp v\xE0o c\xE0i \u0111\u1EB7t th\u1EDDi gian v\xE0 ng\xE0\
  y c\u1EE7a h\u1EC7 th\u1ED1ng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111\
  i\u1EC1u n\xE0y \u0111\u1EC3 ghi th\u1EDDi gian c\u1EE7a s\u1EF1\u2026"
lastmod: '2024-03-11T00:14:10.413924-06:00'
model: gpt-4-0125-preview
summary: "Trong Swift, vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i \u0111\xF2i h\u1ECF\
  i truy c\u1EADp v\xE0o c\xE0i \u0111\u1EB7t th\u1EDDi gian v\xE0 ng\xE0y c\u1EE7\
  a h\u1EC7 th\u1ED1ng. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 ghi th\u1EDDi gian c\u1EE7a s\u1EF1\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
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
