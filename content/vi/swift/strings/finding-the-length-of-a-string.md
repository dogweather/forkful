---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases:
- /vi/swift/finding-the-length-of-a-string.md
date:                  2024-01-28T22:00:40.187871-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm độ dài của một chuỗi nghĩa là xác định xem nó chứa bao nhiêu ký tự. Lập trình viên làm điều này để xác thực đầu vào, thao tác với văn bản, hoặc đơn giản là để hiểu kích thước dữ liệu của họ.

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
