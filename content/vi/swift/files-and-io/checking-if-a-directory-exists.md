---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:19.422090-07:00
description: "L\xE0m th\u1EBF n\xE0o: `FileManager` c\u1EE7a Swift c\xF3 c\xE1c c\xF4\
  ng c\u1EE5 cho vi\u1EC7c n\xE0y. S\u1EED d\u1EE5ng ph\u01B0\u01A1ng th\u1EE9c `fileExists(atPath:)`\
  \ c\u1EE7a n\xF3."
lastmod: '2024-03-13T22:44:37.116732-06:00'
model: gpt-4-0125-preview
summary: "`FileManager` c\u1EE7a Swift c\xF3 c\xE1c c\xF4ng c\u1EE5 cho vi\u1EC7c\
  \ n\xE0y."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Làm thế nào:
`FileManager` của Swift có các công cụ cho việc này. Sử dụng phương thức `fileExists(atPath:)` của nó:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/directory"

if fileManager.fileExists(atPath: path) {
    print("Có, nó ở đó!")
} else {
    print("Không, không tồn tại.")
}
```

Mẫu đầu ra nếu thư mục tồn tại:

```
Có, nó ở đó!
```

Hoặc nếu không:

```
Không, không tồn tại.
```

## Đi sâu vào
Trước `FileManager`, được cung cấp với bộ Foundation, các lệnh UNIX trong scripts thường xuyên được sử dụng để kiểm tra đường dẫn. Nhưng `FileManager` dễ sử dụng và an toàn hơn. Các lựa chọn thay thế trong Swift bao gồm làm việc với lớp `URL` và phương thức `checkResourceIsReachable()` của nó, mặc dù nó phù hợp hơn để kiểm tra khả năng có sẵn của tệp và có thể phát ra lỗi. Nội bộ, `FileManager` sử dụng lệnh gọi hệ thống `stat` để xác minh sự tồn tại của một đường dẫn mà không cần biết liệu nó có phải là một tệp hay thư mục, vì vậy khi bạn cần phân biệt, bạn sẽ phải kiểm tra thêm các thuộc tính của đường dẫn.

## Xem thêm
- Tài liệu Swift: [`FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- Sách Swift: [Làm việc với Thư mục](https://docs.swift.org/swift-book/)
- Diễn đàn Nhà phát triển Apple: [Truy cập Hệ thống Tệp](https://developer.apple.com/forums/tags/file-system/)
