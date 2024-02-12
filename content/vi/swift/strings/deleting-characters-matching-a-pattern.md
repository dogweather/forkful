---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases: - /vi/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:59:29.817677-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
