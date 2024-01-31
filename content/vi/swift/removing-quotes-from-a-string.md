---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:07:03.253987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Loại bỏ dấu ngoặc khỏi một chuỗi có nghĩa là gỡ bỏ bất kỳ dấu ngoặc kép nào bao quanh nội dung. Chúng ta làm điều này để làm sạch dữ liệu đầu vào, chuẩn bị dữ liệu cho việc lưu trữ, hoặc loại bỏ định dạng văn bản không cần thiết có thể gây cản trở quá trình xử lý dữ liệu.

## Cách thực hiện:

Swift cho phép bạn xử lý việc loại bỏ dấu ngoặc một cách khá tiện lợi. Dưới đây là một ví dụ nhanh sử dụng `replacingOccurrences(of:with:)`, làm đúng như cái tên của nó - thay thế các đoạn văn bản bằng thứ khác, hoặc không có gì cả.

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// Xử lý dấu ngoặc đơn? Chỉ cần thay đổi thuật ngữ tìm kiếm.
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

Đầu ra sẽ là các chuỗi không có dấu ngoặc, sẵn sàng cho bất cứ điều gì bạn dự định tiếp theo.

## Sâu hơn

Chúng ta đã bắt đầu "làm sạch" các chuỗi như vậy từ thời kỳ đầu của lập trình. Trong những ngày đầu, việc này chủ yếu liên quan đến việc bảo toàn bộ nhớ quý giá và tránh lỗi cú pháp khi xử lý đầu vào. Bước qua thời đại ngày nay, và nó liên quan đến việc duy trì vệ sinh dữ liệu tốt - đặc biệt là khi xử lý JSON hoặc chuẩn bị chuỗi cho công việc cơ sở dữ liệu. Một dấu ngoặc lạc có thể làm rối loạn truy vấn SQL nhanh hơn bạn nói "lỗi cú pháp."

Các phương án khác? Nếu bạn thấy `replacingOccurrences(of:with:)` hơi quá đơn giản, bạn có thể tìm hiểu về biểu thức thông thường để xử lý các mẫu phức tạp hơn hoặc khi bạn muốn loại bỏ dấu ngoặc chỉ ở những vị trí cụ thể. Lớp `NSRegularExpression` của Swift sẽ là bạn thân ở đây. Nhưng nhớ rằng, regex có thể là một con dao hai lưỡi - mạnh mẽ nhưng đôi khi là quá mức cần thiết.

Về mặt triển khai, `replacingOccurrences(of:with:)` là một phương thức do `String` trong Swift cung cấp, nó gọi nội bộ các hàm xử lý chuỗi phức tạp hơn xử lý Unicode và các phức tạp khác của việc xử lý văn bản hiện đại. Đó là một trong những trường hợp "đơn giản bề ngoài, phức tạp bên trong" mà Swift xử lý để bạn không cần phải làm vậy.

## Tham Khảo Thêm

Để biết thêm về việc xử lý chuỗi trong Swift:

- Ngôn ngữ lập trình Swift (Strings and Characters): [Tài liệu Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Tài liệu Apple Developer](https://developer.apple.com/documentation/foundation/nsregularexpression)

Và nếu bạn bây giờ tò mò về biểu thức thông thường và muốn kiểm tra các mẫu của mình:

- Regex101: [Công cụ kiểm tra và gỡ lỗi Regex](https://regex101.com)
