---
title:                "Tạo một tập tin tạm thời"
aliases: - /vi/swift/creating-a-temporary-file.md
date:                  2024-01-28T21:58:52.239083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo một tệp tạm thời là để tạo ra một tệp chỉ tồn tại trong thời gian ngắn để lưu trữ dữ liệu mà không cần giữ lâu dài. Lập trình viên thực hiện việc này để xử lý dữ liệu chỉ liên quan trong suốt thời gian thực thi của chương trình hoặc để tránh làm đầy bộ nhớ của người dùng bằng những tệp không cần thiết.

## Làm thế nào:
Swift làm cho việc tạo tệp tạm thời trở nên khá dễ dàng, sử dụng lớp `FileManager`. Dưới đây là cách bạn tạo một tệp tạm thời và viết một số văn bản vào trong đó:

```Swift
import Foundation

// Tạo một URL thư mục tạm thời
let tempDirectoryURL = FileManager.default.temporaryDirectory

// Tạo một tên tệp duy nhất
let fileName = UUID().uuidString

// Xây dựng URL tệp đầy đủ
let fileURL = tempDirectoryURL.appendingPathComponent(fileName)

// Văn bản mẫu để viết
let sampleText = "Xin chào, thế giới tạm thời!"

do {
    // Viết văn bản vào tệp tạm thời
    try sampleText.write(to: fileURL, atomically: true, encoding: .utf8)
    print("Tệp đã được tạo: \(fileURL)")
} catch {
    print("Không thể viết vào tệp: \(error)")
}

// Ví dụ đầu ra:
// Tệp đã được tạo: file:///đường/dẫn/tới/thư/mục/tạm/E0B4952E-5BEE-47E7-B5BB-DA5E6AF1EDC9
```

Để đọc tệp, chỉ cần làm ngược lại—dưới đây là cách:

```Swift
do {
    // Đọc văn bản từ tệp tạm thời
    let savedText = try String(contentsOf: fileURL, encoding: .utf8)
    print("Nội dung tệp: \(savedText)")
} catch {
    print("Không thể đọc tệp: \(error)")
}

// Ví dụ đầu ra:
// Nội dung tệp: Xin chào, thế giới tạm thời!
```

Dọn dẹp sau khi sử dụng bằng cách xóa tệp tạm:

```Swift
do {
    // Xóa tệp tạm thời
    try FileManager.default.removeItem(at: fileURL)
    print("Tệp tạm thời đã được xóa.")
} catch {
    print("Không thể xóa tệp: \(error)")
}

// Ví dụ đầu ra:
// Tệp tạm thời đã được xóa.
```

## Sâu hơn
Trước `FileManager`, mọi người quản lý tệp theo cách khó khăn hơn. Nhớ `tmpfile()` của C không? `FileManager` của Swift so với vậy là một làn gió mới: đơn giản và hiện đại.

Có sự thay thế không? Chắc chắn. Bạn có thể sử dụng các biểu diễn trong bộ nhớ như `Data` hoặc `String`, hoàn hảo cho dữ liệu tạm thời thực sự với kích thước hạn chế. Một con đường khác là sử dụng một trình quản lý tệp tạm thời tùy chỉnh để có nhiều quyền kiểm soát hơn, nhưng đó thường là quá mức cần thiết.

Nội dung chi tiết: `FileManager` sử dụng thư mục tạm thời của hệ thống, được dọn dẹp định kỳ nhưng không sau mỗi lần chạy chương trình. Hãy nhớ điều này khi nói đến bảo mật hoặc dữ liệu nhạy cảm—dọn dẹp thủ công nếu cần.

## Xem thêm
Hãy xem những tài liệu này để biết thêm về cách xử lý tệp trong Swift:
- [Tài liệu FileManager của Apple](https://developer.apple.com/documentation/foundation/filemanager)
- [Bài viết về Quản lý Tệp trên NSHipster](https://nshipster.com/temporary-files/)
- [Hướng dẫn của Ray Wenderlich về làm việc với hệ thống tệp trong Swift](https://www.raywenderlich.com/666-filemanager-class-tutorial-for-macos-getting-started)
