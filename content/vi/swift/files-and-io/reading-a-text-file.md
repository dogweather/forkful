---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:18.674562-07:00
description: "C\xE1ch th\u1EE9c: \u0110\u1EC3 \u0111\u1ECDc v\u0103n b\u1EA3n t\u1EEB\
  \ m\u1ED9t t\u1EC7p trong Swift, s\u1EED d\u1EE5ng c\xE1c ph\u01B0\u01A1ng th\u1EE9\
  c ti\u1EC7n \xEDch c\u1EE7a l\u1EDBp `String`. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5 nh\u1ECF."
lastmod: '2024-03-13T22:44:37.120598-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 \u0111\u1ECDc v\u0103n b\u1EA3n t\u1EEB m\u1ED9t t\u1EC7p trong\
  \ Swift, s\u1EED d\u1EE5ng c\xE1c ph\u01B0\u01A1ng th\u1EE9c ti\u1EC7n \xEDch c\u1EE7\
  a l\u1EDBp `String`."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Cách thức:
Để đọc văn bản từ một tệp trong Swift, sử dụng các phương thức tiện ích của lớp `String`. Dưới đây là một ví dụ nhỏ:

```Swift
import Foundation

if let filePath = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let nội dung = try String(contentsOfFile: filePath, encoding: .utf8)
        print(nội dung)
    } catch {
        print("Oops! Có gì đó không ổn: \(error)")
    }
}
```
Nếu "example.txt" chứa "Hello, world!", kết quả là:
```
Hello, world!
```

## Đào Sâu
Đọc các tệp văn bản là việc làm từ thời xa xưa trong thế giới lập trình. Ngay từ đầu, nó liên quan đến các thẻ đục và băng từ. Bây giờ, với các ngôn ngữ cấp cao như Swift, nó trở nên đơn giản. Đoạn mã trên sử dụng `String(contentsOfFile:)`, nhưng có các lựa chọn khác:

- `FileManager`: Tốt cho các thao tác tệp phức tạp hơn.
- `InputStream`: Sử dụng nó khi làm việc với các tệp lớn - ít tốn bộ nhớ hơn.
- `URLSession`: Lấy các tệp từ máy chủ từ xa.

Phương pháp `String(contentsOfFile:)` có thể tốn nhiều bộ nhớ nếu được sử dụng với các tệp kích thước lớn. Để tránh vấn đề, xem xét các phương pháp dựa trên luồng hoặc đọc theo từng phần.

## Xem Thêm
Hãy tìm hiểu trong tài liệu chính thức của Swift:
- [String](https://developer.apple.com/documentation/swift/string)
- [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Làm việc với URL Session](https://developer.apple.com/documentation/foundation/url_loading_system/fetching_website_data_into_memory)

Để hiểu sâu hơn, xem qua các nguồn tài liệu này:
- [Hướng dẫn Lập trình Hệ thống Tệp của Apple](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
