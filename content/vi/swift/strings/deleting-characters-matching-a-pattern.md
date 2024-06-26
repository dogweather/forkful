---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:29.817677-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Tr\u01B0\u1EDBc Swift v\xE0 l\u1EADp tr\xEC\
  nh hi\u1EC7n \u0111\u1EA1i, vi\u1EC7c kh\u1EDBp m\u1EABu l\xE0 l\u0129nh v\u1EF1\
  c c\u1EE7a nh\u1EEFng c\xF4ng c\u1EE5 v\xE0 ng\xF4n ng\u1EEF c\u1EE5 th\u1EC3 nh\u01B0\
  \ `sed`, `awk`, hay Perl n\u1ED5i ti\u1EBFng v\u1EDBi\u2026"
lastmod: '2024-04-05T21:53:38.430709-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc Swift v\xE0 l\u1EADp tr\xECnh hi\u1EC7n \u0111\u1EA1i, vi\u1EC7\
  c kh\u1EDBp m\u1EABu l\xE0 l\u0129nh v\u1EF1c c\u1EE7a nh\u1EEFng c\xF4ng c\u1EE5\
  \ v\xE0 ng\xF4n ng\u1EEF c\u1EE5 th\u1EC3 nh\u01B0 `sed`, `awk`, hay Perl n\u1ED5\
  i ti\u1EBFng v\u1EDBi kh\u1EA3 n\u0103ng x\u1EED l\xFD v\u0103n b\u1EA3n."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

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
