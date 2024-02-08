---
title:                "Tìm kiếm và thay thế văn bản"
aliases:
- vi/swift/searching-and-replacing-text.md
date:                  2024-01-28T22:07:33.113352-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Tìm kiếm và thay thế văn bản trong lập trình chính xác là như cái tên của nó: quét các chuỗi để tìm kiếm các mẫu nhất định và thay thế chúng bằng cái khác. Lập trình viên thực hiện điều này khá nhiều - cho việc dọn dẹp dữ liệu, cập nhật giao diện người dùng, hoặc chuẩn bị chuỗi cho việc xử lý.

## Cách thực hiện:

```Swift
var greetings = "Hello, old friend!"

// Thay thế đơn giản
greetings = greetings.replacingOccurrences(of: "old", with: "new")
print(greetings) // "Hello, new friend!"

// Sử dụng các tùy chọn để thay thế không phân biệt chữ hoa chữ thường
let caseInsensitiveResult = greetings.replacingOccurrences(
    of: "hello",
    with: "Hi",
    options: .caseInsensitive
)
print(caseInsensitiveResult) // "Hi, new friend!"

// Thay thế bằng biểu thức chính quy
let regexResult = greetings.replacingOccurrences(
    of: "\\bnew\\b",
    with: "best",
    options: .regularExpression
)
print(regexResult) // "Hello, best friend!"
```

## Sâu hơn

Chúng ta đã thay thế văn bản trong chuỗi kể từ những ngày đầu của máy tính. Ban đầu, nó được thực hiện với các công cụ dòng lệnh đơn giản như `sed`. Trong Swift, `replacingOccurrences(of:with:)` thực hiện công việc nặng nhọc, và bạn có thêm sự kiểm soát với các tùy chọn như `.caseInsensitive` hay `.regularExpression`.

Các phương án thay thế trong Swift bao gồm sử dụng `NSRegularExpression` cho các mẫu phức tạp và `NSMutableString` cho các thao tác chuỗi có thể thay đổi. Bên dưới, các phương pháp thay thế chuỗi của Swift được kết nối với các bản đối tượng Objective-C mạnh mẽ, cung cấp tốc độ và đa dạng.

## Xem thêm

- [Tài liệu String Swift](https://developer.apple.com/documentation/swift/string/)
- [Biểu thức chính quy trong Swift](https://nshipster.com/swift-regular-expressions/)
- [Swift.org - Làm việc với Chuỗi](https://swift.org/documentation/api-design-guidelines/#strive-for-fluent-usage)
