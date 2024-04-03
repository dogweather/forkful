---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:06.313271-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EBFt v\u0103n b\u1EA3n v\xE0o m\u1ED9t\
  \ t\u1EC7p trong Swift r\u1EA5t d\u1EC5 d\xE0ng v\u1EDBi l\u1EDBp `String` v\xE0\
  \ `FileManager`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:37.122043-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt v\u0103n b\u1EA3n v\xE0o m\u1ED9t t\u1EC7p trong Swift r\u1EA5\
  t d\u1EC5 d\xE0ng v\u1EDBi l\u1EDBp `String` v\xE0 `FileManager`."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
Viết văn bản vào một tệp trong Swift rất dễ dàng với lớp `String` và `FileManager`. Dưới đây là một ví dụ nhanh:

```Swift
import Foundation

let stringToWrite = "Xin chào, Swift!"
let fileURL = FileManager.default.urls(for: .documentDirectory, trong: .userDomainMask).first?.appendingPathComponent("ví_dụ.txt")

try {
    try stringToWrite.write(to: fileURL!, atomically: true, encoding: .utf8)
    print("Tệp đã được viết thành công")
} catch {
    print("Lỗi khi viết vào tệp: \(error)")
}
```

Kết quả Mẫu:
```
Tệp đã được viết thành công
```

## Khám Phá Sâu
Việc viết tệp văn bản cũ kỹ như chính những chiếc máy tính vậy, thường được sử dụng cho việc lưu trữ dữ liệu nhỏ trước khi cơ sở dữ liệu trở nên phổ biến. Các phương án thay thế chính bao gồm cơ sở dữ liệu và mặc định của người dùng, chúng được cấu trúc và hiệu quả hơn cho các bộ dữ liệu lớn hơn. Khi viết tệp trong Swift, phương thức `write(to:atomically:encoding:)` đảm bảo các lần ghi một cách nguyên tử, ngăn chặn sự hỏng hóc dữ liệu trong quá trình ghi.

## Xem Thêm
- Tài liệu Swift String: https://developer.apple.com/documentation/swift/string
- Hướng dẫn về FileManager: https://developer.apple.com/documentation/foundation/filemanager
- Làm việc với JSON trong Swift: https://developer.apple.com/swift/blog/?id=37
- Hướng dẫn Xử lý Tệp trong Swift: https://www.raywenderlich.com/1881-file-handling-in-swift-tutorial
