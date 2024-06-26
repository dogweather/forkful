---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:53.966960-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift l\xE0m cho vi\u1EC7c l\xE0m vi\u1EC7c\
  \ v\u1EDBi c\xE1c chu\u1ED7i con kh\xE1 \u0111\u01A1n gi\u1EA3n. H\xE3y nh\u1EA3\
  y v\xE0o ngay v\u1EDBi m\u1ED9t s\u1ED1 v\xED d\u1EE5."
lastmod: '2024-03-13T22:44:37.082232-06:00'
model: gpt-4-0125-preview
summary: "Swift l\xE0m cho vi\u1EC7c l\xE0m vi\u1EC7c v\u1EDBi c\xE1c chu\u1ED7i con\
  \ kh\xE1 \u0111\u01A1n gi\u1EA3n."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

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
