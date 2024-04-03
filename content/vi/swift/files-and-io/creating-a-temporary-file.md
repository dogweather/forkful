---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:52.239083-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift l\xE0m cho vi\u1EC7c t\u1EA1o t\u1EC7\
  p t\u1EA1m th\u1EDDi tr\u1EDF n\xEAn kh\xE1 d\u1EC5 d\xE0ng, s\u1EED d\u1EE5ng l\u1EDB\
  p `FileManager`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch b\u1EA1n t\u1EA1o m\u1ED9\
  t t\u1EC7p t\u1EA1m th\u1EDDi v\xE0 vi\u1EBFt m\u1ED9t s\u1ED1\u2026"
lastmod: '2024-03-13T22:44:37.123335-06:00'
model: gpt-4-0125-preview
summary: "Swift l\xE0m cho vi\u1EC7c t\u1EA1o t\u1EC7p t\u1EA1m th\u1EDDi tr\u1EDF\
  \ n\xEAn kh\xE1 d\u1EC5 d\xE0ng, s\u1EED d\u1EE5ng l\u1EDBp `FileManager`."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

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
