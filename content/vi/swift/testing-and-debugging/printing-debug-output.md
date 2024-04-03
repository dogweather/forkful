---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:50.703974-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Swift, b\u1EA1n c\xF3 m\u1ED9t ng\u01B0\
  \u1EDDi b\u1EA1n trong h\xE0m `print()`. D\u1EC5 s\u1EED d\u1EE5ng, n\xF3 cho b\u1EA1\
  n c\xE1i nh\xECn v\u1EC1 nh\u1EEFng g\xEC \u0111ang di\u1EC5n ra trong m\xE3 c\u1EE7\
  a b\u1EA1n."
lastmod: '2024-03-13T22:44:37.100669-06:00'
model: gpt-4-0125-preview
summary: "Trong Swift, b\u1EA1n c\xF3 m\u1ED9t ng\u01B0\u1EDDi b\u1EA1n trong h\xE0\
  m `print()`."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Làm thế nào:
Trong Swift, bạn có một người bạn trong hàm `print()`. Dễ sử dụng, nó cho bạn cái nhìn về những gì đang diễn ra trong mã của bạn.

```Swift
var greeting = "Hello, playground"
print(greeting)
// Đầu ra: Hello, playground

let numbers = [1, 2, 3, 4, 5]
for number in numbers {
    print(number)
}
// Đầu ra:
// 1
// 2
// 3
// 4
// 5
```

Nhưng chờ đã, còn nhiều hơn nữa! Cần thông tin gỡ lỗi chi tiết? `debugPrint()` sẽ giúp bạn:

```Swift
debugPrint(greeting)
// Đầu ra: "Hello, playground"
```

Thấy những dấu ngoặc kép không? `debugPrint()` cung cấp thông tin thêm về các loại dữ liệu và cấu trúc.

## Sâu hơn
Trong những ngày đầu của Objective-C, chúng tôi sử dụng `NSLog` để ghi nhật ký. Swift giữ mọi thứ đơn giản—`print()` là bánh mì và bơ của bạn cho đầu ra tiêu chuẩn, trong khi `debugPrint()` là bơ có hương vị cho các cái nhìn chi tiết.

Thông tin thú vị: Đầu ra tiêu chuẩn trong Swift không chỉ là văn bản—nó có thể là bất kỳ loại nào tuân theo `CustomStringConvertible` hoặc `CustomDebugStringConvertible`. Những giao thức này cho phép bạn tùy chỉnh cách các đối tượng của mình trông như thế nào khi chúng kể câu chuyện qua việc in.

Bên trong, `print()` và `debugPrint()` sử dụng `String(describing:)` và `String(reflecting:)` để biến đối tượng của bạn thành chuỗi. Cơ bản, những hàm này sử dụng một chiếc gương để chụp selfie dữ liệu của bạn.

Có phương án khác không? Bạn có `os_log` và `NSLog`, nhưng những cái này phù hợp hơn cho việc ghi nhật ký ở cấp độ sản xuất, không phải quá trình gỡ lỗi nhanh gọn mà chúng ta đang đề cập ở đây.

## Xem thêm
- Tài liệu tham khảo API của Swift về các hàm in: [Thư viện tiêu chuẩn Swift: print(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541053-print)
- Cái nhìn sâu hơn về việc ghi nhật ký trong Swift, GDPR và các xem xét về quyền riêng tư: [Unified Logging and Activity Tracing](https://developer.apple.com/documentation/os/logging)
- Sự nội suy chuỗi và khả năng tùy chỉnh cho các mô tả gỡ lỗi của Swift: [CustomStringConvertible](https://developer.apple.com/documentation/swift/customstringconvertible) và [CustomDebugStringConvertible](https://developer.apple.com/documentation/swift/customdebugstringconvertible)
