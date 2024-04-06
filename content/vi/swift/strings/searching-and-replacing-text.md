---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:33.113352-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ch\xFAng ta \u0111\xE3 thay th\u1EBF v\u0103\
  n b\u1EA3n trong chu\u1ED7i k\u1EC3 t\u1EEB nh\u1EEFng ng\xE0y \u0111\u1EA7u c\u1EE7\
  a m\xE1y t\xEDnh. Ban \u0111\u1EA7u, n\xF3 \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7\
  n v\u1EDBi c\xE1c c\xF4ng c\u1EE5 d\xF2ng l\u1EC7nh \u0111\u01A1n gi\u1EA3n\u2026"
lastmod: '2024-04-05T21:53:38.432104-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta \u0111\xE3 thay th\u1EBF v\u0103n b\u1EA3n trong chu\u1ED7i\
  \ k\u1EC3 t\u1EEB nh\u1EEFng ng\xE0y \u0111\u1EA7u c\u1EE7a m\xE1y t\xEDnh."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

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
