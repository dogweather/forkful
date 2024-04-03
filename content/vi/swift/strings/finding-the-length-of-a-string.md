---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:40.187871-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Swift, b\u1EA1n c\xF3 \u0111\u01B0\
  \u1EE3c \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i b\u1EB1ng c\xE1ch truy\
  \ c\u1EADp v\xE0o thu\u1ED9c t\xEDnh `count` c\u1EE7a n\xF3. Th\u1EB3ng th\u1EAF\
  n, h\xE3y l\xE0m \u0111i\u1EC1u \u0111\xF3."
lastmod: '2024-03-13T22:44:37.084849-06:00'
model: gpt-4-0125-preview
summary: "Trong Swift, b\u1EA1n c\xF3 \u0111\u01B0\u1EE3c \u0111\u1ED9 d\xE0i c\u1EE7\
  a m\u1ED9t chu\u1ED7i b\u1EB1ng c\xE1ch truy c\u1EADp v\xE0o thu\u1ED9c t\xEDnh\
  \ `count` c\u1EE7a n\xF3."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

## Cách thực hiện:
Trong Swift, bạn có được độ dài của một chuỗi bằng cách truy cập vào thuộc tính `count` của nó. Thẳng thắn, hãy làm điều đó:

```Swift
let greeting = "Hello, World!"
print(greeting.count) // Đầu ra: 13
```

Hãy nhớ rằng Swift xem emoji như là những ký tự đơn, nhờ vào Unicode:

```Swift
let wave = "👋"
print(wave.count)  // Đầu ra: 1
```

## Sâu hơn
Trở lại thời Objective-C, việc tìm độ dài của chuỗi không được trực tiếp như vậy—có `length` và `lengthOfBytes(using:)`. Swift đã làm cho nó trở nên sạch sẽ hơn với `count`.

Hãy cảnh giác với các ký tự tổ hợp: những ký tự đơn trông có vẻ như được tạo từ nhiều bộ chữ số Unicode. `count` xử lý những điều này một cách duyên dáng.

Có phương án thay thế không? Chắc chắn rồi, bạn có thể đi qua chuỗi bằng vòng lặp, nhưng đó là việc tái phát minh cái bánh xe và kém hiệu quả hơn.

Về cơ bản, `count` là O(n), nơi ‘n’ là số lượng ký tự. Điều này bởi vì `String` của Swift không phải là tập hợp của `Char`s, mà là một dãy của các cụm đồ họa, có thể thay đổi độ dài.

## Xem thêm
- Tài liệu Swift về Chuỗi: [Tài liệu Chuỗi Swift](https://developer.apple.com/documentation/swift/string)
- Cơ bản về Unicode: [Liên đoàn Unicode](https://home.unicode.org)
- Tìm hiểu về Hiệu suất Chuỗi Swift: [Hiệu suất Chuỗi Swift](https://swift.org/blog/utf8-string/)
