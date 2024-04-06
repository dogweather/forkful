---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:47.484043-07:00
description: "L\xE0m th\u1EBF n\xE0o: Tr\u01B0\u1EDBc kia, trong c\xE1c ng\xF4n ng\u1EEF\
  \ l\u1EADp tr\xECnh nh\u01B0 C, ng\u01B0\u1EDDi ta ph\u1EA3i th\u1EE7 c\xF4ng di\
  \ chuy\u1EC3n chu\u1ED7i v\u1EDBi c\xE1c h\xE0m, \u0111\u1ED1i ph\xF3 v\u1EDBi m\u1EA3\
  ng v\xE0 chu\u1ED7i k\u1EBFt th\xFAc b\u1EB1ng\u2026"
lastmod: '2024-04-05T21:53:38.440544-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc kia, trong c\xE1c ng\xF4n ng\u1EEF l\u1EADp tr\xECnh nh\u01B0\
  \ C, ng\u01B0\u1EDDi ta ph\u1EA3i th\u1EE7 c\xF4ng di chuy\u1EC3n chu\u1ED7i v\u1EDB\
  i c\xE1c h\xE0m, \u0111\u1ED1i ph\xF3 v\u1EDBi m\u1EA3ng v\xE0 chu\u1ED7i k\u1EBF\
  t th\xFAc b\u1EB1ng null."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

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
