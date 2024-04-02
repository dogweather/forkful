---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:29.817677-07:00
description: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu ngh\u0129\
  a l\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i k\xFD t\u1EF1 c\u1EE5 th\u1EC3 kh\u1ECF\
  i m\u1ED9t chu\u1ED7i d\u1EF1a tr\xEAn m\u1ED9t m\u1EABu \u0111\xE3 \u0111\u01B0\
  \u1EE3c \u0111\u1ECBnh ngh\u0129a, nh\u01B0 s\u1ED1 ho\u1EB7c d\u1EA5u ch\u1EA5\
  m c\xE2u. L\u1EADp\u2026"
lastmod: '2024-03-13T22:44:37.075709-06:00'
model: gpt-4-0125-preview
summary: "X\xF3a c\xE1c k\xFD t\u1EF1 kh\u1EDBp v\u1EDBi m\u1ED9t m\u1EABu ngh\u0129\
  a l\xE0 lo\u1EA1i b\u1ECF c\xE1c chu\u1ED7i k\xFD t\u1EF1 c\u1EE5 th\u1EC3 kh\u1ECF\
  i m\u1ED9t chu\u1ED7i d\u1EF1a tr\xEAn m\u1ED9t m\u1EABu \u0111\xE3 \u0111\u01B0\
  \u1EE3c \u0111\u1ECBnh ngh\u0129a, nh\u01B0 s\u1ED1 ho\u1EB7c d\u1EA5u ch\u1EA5\
  m c\xE2u. L\u1EADp\u2026"
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

## Lý do & Tại sao?

Xóa các ký tự khớp với một mẫu nghĩa là loại bỏ các chuỗi ký tự cụ thể khỏi một chuỗi dựa trên một mẫu đã được định nghĩa, như số hoặc dấu chấm câu. Lập trình viên thực hiện điều này để làm sạch đầu vào, dọn dẹp dữ liệu, hoặc chuẩn bị cho việc xử lý nơi mà những mẫu cụ thể không cần thiết.

## Cách thực hiện:

```swift
import Foundation

// Ví dụ: Loại bỏ tất cả số từ một chuỗi
let originalString = "Contact me at 123-456-7890 after 09:00 PM."
let digitsPattern = "[0-9]"
let resultString = originalString.replacingOccurrences(of: digitsPattern, with: "", options: .regularExpression)

print(resultString)  // Đầu ra: "Contact me at -- after : PM."
```

```swift
// Ví dụ: Loại bỏ các ký tự không phải là chữ và số
let messyString = "H3!llo, W%@rld-"
let nonAlphanumericPattern = "[^A-Za-z0-9]"
let cleanString = messyString.replacingOccurrences(of: nonAlphanumericPattern, with: "", options: .regularExpression)

print(cleanString)  // Đầu ra: "H3lloWrld"
```

## Sâu hơn

Trước Swift và lập trình hiện đại, việc khớp mẫu là lĩnh vực của những công cụ và ngôn ngữ cụ thể như `sed`, `awk`, hay Perl nổi tiếng với khả năng xử lý văn bản. Swift, với bộ khung Robust Foundation của mình, đơn giản hóa những nhiệm vụ này trong ngôn ngữ, làm cho nó dễ tiếp cận hơn với các nhà phát triển.

Một phương án thay thế cho biểu thức chính quy là lặp qua chuỗi bằng phương pháp `filter` của Swift kết hợp với điều kiện tùy chỉnh, có thể cũng tốn nhiều thời gian và ít dễ đọc hơn. Biểu thức chính quy cung cấp một cách mô tả compact, mặc dù đôi khi là khó hiểu, về mẫu chúng tôi muốn loại bỏ hoặc thao tác.

Phía sau hậu trường, khi bạn chạy `replacingOccurrences(of:with:options:)` với tùy chọn `.regularExpression`, Swift sử dụng động cơ biểu thức chính quy của ICU (International Components for Unicode) để xử lý mẫu. ICU là một thư viện sử dụng rộng rãi, đã đạt đến tuổi mature, cho việc hỗ trợ Unicode, bao gồm khớp mẫu, được xây dựng vào nhiều ngôn ngữ lập trình cấp cao.

## Xem thêm

- Tài liệu về Chuỗi Swift: https://developer.apple.com/documentation/swift/string
- Biểu thức chính quy trong Swift: https://developer.apple.com/documentation/foundation/nsregularexpression
- Hướng dẫn sử dụng ICU cho Biểu thức chính quy: https://unicode-org.github.io/icu/userguide/strings/regexp.html
