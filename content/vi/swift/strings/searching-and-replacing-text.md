---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:33.113352-07:00
description: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n trong l\u1EAD\
  p tr\xECnh ch\xEDnh x\xE1c l\xE0 nh\u01B0 c\xE1i t\xEAn c\u1EE7a n\xF3: qu\xE9t\
  \ c\xE1c chu\u1ED7i \u0111\u1EC3 t\xECm ki\u1EBFm c\xE1c m\u1EABu nh\u1EA5t \u0111\
  \u1ECBnh v\xE0 thay th\u1EBF ch\xFAng b\u1EB1ng c\xE1i\u2026"
lastmod: '2024-03-13T22:44:37.077001-06:00'
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n trong l\u1EADp tr\xEC\
  nh ch\xEDnh x\xE1c l\xE0 nh\u01B0 c\xE1i t\xEAn c\u1EE7a n\xF3: qu\xE9t c\xE1c chu\u1ED7\
  i \u0111\u1EC3 t\xECm ki\u1EBFm c\xE1c m\u1EABu nh\u1EA5t \u0111\u1ECBnh v\xE0 thay\
  \ th\u1EBF ch\xFAng b\u1EB1ng c\xE1i\u2026"
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
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
