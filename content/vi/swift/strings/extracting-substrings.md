---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:53.966960-07:00
description: "Tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 ch\u1EC9\
  \ l\u1EA5y m\u1ED9t ph\u1EA7n c\u1EE7a chu\u1ED7i\u2014gi\u1ED1ng nh\u01B0 c\u1EAF\
  t m\u1ED9t d\u1EA3i ruy b\u0103ng v\u1EDBi \u0111\u1ED9 d\xE0i b\u1EA1n c\u1EA7\
  n. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xE1\
  ch\u2026"
lastmod: 2024-02-19 22:04:56.287760
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 ch\u1EC9\
  \ l\u1EA5y m\u1ED9t ph\u1EA7n c\u1EE7a chu\u1ED7i\u2014gi\u1ED1ng nh\u01B0 c\u1EAF\
  t m\u1ED9t d\u1EA3i ruy b\u0103ng v\u1EDBi \u0111\u1ED9 d\xE0i b\u1EA1n c\u1EA7\
  n. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 t\xE1\
  ch\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trích xuất các chuỗi con có nghĩa là chỉ lấy một phần của chuỗi—giống như cắt một dải ruy băng với độ dài bạn cần. Các lập trình viên làm điều này để tách biệt, phân tích, hoặc xử lý các phần văn bản cụ thể, như đầu vào của người dùng, tên tệp tin, hoặc xử lý văn bản.

## Làm thế nào:

Swift làm cho việc làm việc với các chuỗi con khá đơn giản. Hãy nhảy vào ngay với một số ví dụ.

```swift
let fullString = "Hello, Swift Programmer!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 12)

// Trích xuất một chuỗi con sử dụng String.Index
let substring = fullString[startIndex...endIndex]

print(substring) // "Swift"

// Một cách khác, sử dụng NSRange và NSString
import Foundation

let nsRange = NSRange(location: 7, length: 6)
if let range = Range(nsRange, in: fullString) {
    let substring = fullString[range]
    print(substring) // "Swift"
}

// Cách ngắn nếu bạn biết chỉ số chính xác
let quickSubstring = fullString[7...12]

print(quickSubstring) // Điều này sẽ báo lỗi vì chuỗi Swift không hỗ trợ chỉ số nguyên
```

Kết quả:
```
Swift
Swift
// Lỗi: 'subscript(_:)' không khả dụng: không thể chỉ mục chuỗi bằng một số nguyên, xem tài liệu của Chuỗi để biết thêm thông tin
```

## Đào sâu

Việc trích xuất chuỗi con trong Swift đòi hỏi phải hiểu cách Swift xử lý chuỗi, khác biệt một chút so với các ngôn ngữ như Python hay C#. Trong Swift, chuỗi là các bộ sưu tập của các ký tự không sử dụng chỉ số nguyên. Điều này xuất phát từ việc Swift hỗ trợ ký tự tuân thủ Unicode, khiến các chuỗi không cố định chiều dài, mà thay vào đó là một bộ sưu tập của các cụm grapheme (những gì người dùng cảm nhận như một ký tự đơn).

Thiết kế này có nghĩa là không thể trực tiếp sử dụng chỉ số nguyên với chuỗi Swift; bạn cần làm việc với `String.Index`. Mặc dù không ngay lập tức trực quan như sử dụng số nguyên, nhưng nó xử lý các kịch bản văn bản và emoji một cách nhất quán.

Các phương án khác bao gồm việc sử dụng `NSString` từ Objective-C, như đã thể hiện trong các ví dụ, cho phép sử dụng NSRange, nhưng đó là kiểu cũ và không hợp với phong cách Swift. Kể từ Swift 4, chính String đã nhận được nhiều sự chú ý, với các tùy chọn API phong phú, trực quan hơn để làm việc với các chuỗi con, để lại `NSString` phía sau cho hầu hết các tác vụ.

Các chi tiết thực hiện là rất quan trọng—việc trích xuất chuỗi con ngây thơ có thể dẫn đến tổn thất về hiệu suất do mỗi lời gọi đến `index(_: offsetBy:)` có thể là O(n) khi xử lý với chuỗi tuân thủ Unicode. Thêm vào đó, khi bạn tạo một chuỗi con trong Swift, nó chia sẻ bộ nhớ của chuỗi gốc, làm cho hiệu suất được cải thiện, nhưng đây là điều cần lưu ý nếu bạn muốn thay đổi chuỗi gốc sau này.

## Xem thêm

Để biết thêm về chủ đề này, hãy tham khảo tài liệu chính thức:

- Swift String và Characters: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Hướng dẫn Lập trình Chuỗi: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/)

Hãy thử nghiệm và vui chơi trong môi trường Swift playground để thực sự nắm bắt được cách thức làm việc.
