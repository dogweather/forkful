---
title:                "Viết một tệp văn bản"
aliases:
- /vi/swift/writing-a-text-file.md
date:                  2024-01-28T22:13:06.313271-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Viết một tệp văn bản liên quan đến việc lưu trữ dữ liệu dưới dạng văn bản có thể đọc được, sử dụng một mã hóa ký tự như UTF-8. Lập trình viên làm điều này để ghi nhật ký, bảo quản dữ liệu, hoặc cấu hình.

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
