---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:19.422090-07:00
description: "Trong Swift, vi\u1EC7c ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5\
  c t\u1ED3n t\u1EA1i gi\xFAp b\u1EA1n x\xE1c nh\u1EADn tr\u1EA1ng th\xE1i c\u1EE7\
  a h\u1EC7 th\u1ED1ng t\u1EC7p tr\u01B0\u1EDBc khi b\u1EA1n \u0111\u1ECDc ho\u1EB7\
  c ghi d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
lastmod: '2024-02-25T18:49:35.462588-07:00'
model: gpt-4-0125-preview
summary: "Trong Swift, vi\u1EC7c ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5\
  c t\u1ED3n t\u1EA1i gi\xFAp b\u1EA1n x\xE1c nh\u1EADn tr\u1EA1ng th\xE1i c\u1EE7\
  a h\u1EC7 th\u1ED1ng t\u1EC7p tr\u01B0\u1EDBc khi b\u1EA1n \u0111\u1ECDc ho\u1EB7\
  c ghi d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Trong Swift, việc kiểm tra nếu một thư mục tồn tại giúp bạn xác nhận trạng thái của hệ thống tệp trước khi bạn đọc hoặc ghi dữ liệu. Lập trình viên làm điều này để tránh lỗi, như đọc từ một thư mục không tồn tại, có thể làm cho ứng dụng bị crash hoặc dẫn đến các hoạt động sai lầm.

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
