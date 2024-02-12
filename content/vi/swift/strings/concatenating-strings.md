---
title:                "Nối chuỗi ký tự"
aliases:
- /vi/swift/concatenating-strings.md
date:                  2024-01-28T21:57:47.484043-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Ghép chuỗi là việc kết hợp các chuỗi riêng biệt lại với nhau để tạo ra một chuỗi mới. Lập trình viên thực hiện điều này để kết hợp văn bản một cách linh hoạt, như tổng hợp lời chào, thông điệp, hay chỉ đơn giản là để cấu trúc dữ liệu một cách dễ đọc.

## Làm thế nào:
```Swift
let firstName = "Taylor"
let lastName = "Swift"
let fullName = firstName + " " + lastName  // Sử dụng toán tử +
print(fullName)  // Đầu ra: "Taylor Swift"

let age = 31
let greeting = "Hello, \(firstName)! Bạn \(age) tuổi."  // Sử dụng nội suy chuỗi
print(greeting)  // Đầu ra: "Hello, Taylor! Bạn 31 tuổi."

var message = "Đây"
message += " là" // Sử dụng toán tử += để thêm vào chuỗi
message += " Sparta!"
print(message)  // Đầu ra: "Đây là Sparta!"
```

## Sâu hơn
Trước kia, trong các ngôn ngữ lập trình như C, người ta phải thủ công di chuyển chuỗi với các hàm, đối phó với mảng và chuỗi kết thúc bằng null. Swift đã làm việc này trở nên dễ dàng. Toán tử '+' cho chuỗi được lấy cảm hứng từ các ngôn ngữ như Java và C++, mang lại cách thức quen thuộc để kết hợp chuỗi.

Có những lựa chọn khác ngoài '+'. Nội suy chuỗi trong Swift không chỉ là để trở nên phô trương - đó là một cách an toàn về kiểu dữ liệu để nhúng giá trị trực tiếp trong chuỗi của bạn. Không cần phải chuyển đổi kiểu dữ liệu hay lo lắng về việc bạn sẽ nhầm lẫn điều gì đó.

Ghép chuỗi nâng cao không chỉ đơn giản là tung hứng từ ngữ. Khi hiệu suất là chìa khóa, sử dụng '+=' một cách liều lĩnh có thể làm chậm bạn lại. Tại sao? Bởi vì nếu bạn thêm vào một chuỗi trong một vòng lặp, Swift có thể tạo ra các chuỗi mới mỗi lần, điều này không mấy nhanh nhẹn. Thay vào đó, hãy xem xét sử dụng 'join()' hoặc 'append()' của 'String' để tăng hiệu quả, đặc biệt với dữ liệu lớn hoặc các vòng lặp phức tạp.

```Swift
// Ghép chuỗi hiệu quả với `join()`
let words = ["Một", "lần", "nào", "đó"]
let story = words.joined(separator: " ")  // Hiệu quả cho việc ghép các phần tử mảng
print(story)  // Đầu ra: "Một lần nào đó"

// Sử dụng 'append(contentsOf:)' để thêm các chuỗi con
var quote = "Tôi nghĩ, "
quote.append(contentsOf: "vì vậy tôi tồn tại")
print(quote)  // Đầu ra: "Tôi nghĩ, vì vậy tôi tồn tại"
```

## Xem thêm
- Tài liệu Swift về Chuỗi: [Tài liệu Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Hướng dẫn Lập trình với Chuỗi của Apple: [Tài liệu cho Nhà phát triển Apple](https://developer.apple.com/documentation/swift/string)
