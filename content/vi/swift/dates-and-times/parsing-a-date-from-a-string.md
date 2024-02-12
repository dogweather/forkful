---
title:                "Phân tích ngày từ chuỗi kí tự"
aliases: - /vi/swift/parsing-a-date-from-a-string.md
date:                  2024-01-28T22:04:40.793495-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Phân tích cú pháp một ngày từ một chuỗi có nghĩa là chuyển đổi biểu diễn văn bản của một ngày (như "2023-04-01") thành một đối tượng Ngày. Các lập trình viên làm điều này để thao tác với ngày, thực hiện tính toán hoặc hiển thị theo các định dạng khác nhau.

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
