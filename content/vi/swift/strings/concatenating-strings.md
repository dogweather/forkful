---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:47.484043-07:00
description: "Gh\xE9p chu\u1ED7i l\xE0 vi\u1EC7c k\u1EBFt h\u1EE3p c\xE1c chu\u1ED7\
  i ri\xEAng bi\u1EC7t l\u1EA1i v\u1EDBi nhau \u0111\u1EC3 t\u1EA1o ra m\u1ED9t chu\u1ED7\
  i m\u1EDBi. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\
  \ \u0111\u1EC3 k\u1EBFt h\u1EE3p v\u0103n b\u1EA3n m\u1ED9t c\xE1ch linh\u2026"
lastmod: '2024-03-13T22:44:37.086101-06:00'
model: gpt-4-0125-preview
summary: "Gh\xE9p chu\u1ED7i l\xE0 vi\u1EC7c k\u1EBFt h\u1EE3p c\xE1c chu\u1ED7i ri\xEA\
  ng bi\u1EC7t l\u1EA1i v\u1EDBi nhau \u0111\u1EC3 t\u1EA1o ra m\u1ED9t chu\u1ED7\
  i m\u1EDBi. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y\
  \ \u0111\u1EC3 k\u1EBFt h\u1EE3p v\u0103n b\u1EA3n m\u1ED9t c\xE1ch linh\u2026"
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
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
