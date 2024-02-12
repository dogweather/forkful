---
title:                "Đọc một tệp văn bản"
aliases:
- vi/swift/reading-a-text-file.md
date:                  2024-01-28T22:05:18.674562-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Đọc một tệp văn bản trong Swift có nghĩa là lấy nội dung từ một tệp lưu trữ trên đĩa. Các lập trình viên làm điều này để làm việc với dữ liệu đã lưu như cấu hình, nhật ký, hoặc nội dung do người dùng tạo.

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
