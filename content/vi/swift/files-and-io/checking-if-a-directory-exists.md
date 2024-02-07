---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-01-28T21:56:19.422090-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
