---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:43.638429-07:00
description: "L\xE0m th\u1EBF n\xE0o: Swift s\u1EED d\u1EE5ng x\u1EED l\xFD l\u1ED7\
  i v\u1EDBi c\xE1c kh\u1ED1i `do`, `try`, v\xE0 `catch`. H\xE3y xem x\xE9t."
lastmod: '2024-03-13T22:44:37.107770-06:00'
model: gpt-4-0125-preview
summary: "Swift s\u1EED d\u1EE5ng x\u1EED l\xFD l\u1ED7i v\u1EDBi c\xE1c kh\u1ED1\
  i `do`, `try`, v\xE0 `catch`."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

## Làm thế nào:
Swift sử dụng xử lý lỗi với các khối `do`, `try`, và `catch`. Hãy xem xét:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Giả sử chúng ta có một số logic ở đây để kiểm tra xem một tệp có tồn tại và liệu chúng ta có quyền đọc nó không
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Nội dung tệp ở đây"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Whoops! Tệp không tìm thấy.")
} catch FileError.noPermission {
    print("Ah! Không có quyền đọc tệp.")
} catch {
    print("Một lỗi không xác định đã xảy ra.")
}

```

Kết quả mẫu:

```
Whoops! Tệp không tìm thấy.
```

## Sâu hơn
Xử lý lỗi không phải lúc nào cũng mượt mà như bây giờ. Trong Objective-C, bạn sẽ xử lý với các con trỏ đến đối tượng NSError, điều đó cảm thấy cồng kềnh. Bây giờ, chúng ta có một hệ thống tinh tế hơn với enum Swift và giao thức `Error`.

Swift’s `throw` cho phép chúng ta tín hiệu rằng có điều gì đó không ổn. Các khối `do` hành động giống như những lĩnh vực nhận biết lỗi, tiền tố `try` gọi công việc rủi ro, và `catch` xử lý mọi thứ nếu chúng đi vào ngõ cụt.

Optional là một giải pháp thay thế cho những tình huống không hẳn là "lỗi" nhưng vẫn có thể "không có kết quả". Chúng giống như biến Schrödinger—chúng có giá trị hoặc không.

Để sâu hơn, hãy xem xét loại `Result`, đây là những hybrid tinh tế giữa mô hình trả về thông thường và mô hình lỗi.

## Xem thêm
- Hướng dẫn Xử lý Lỗi Swift Chính thức: [Tài liệu Apple](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Các Phương pháp Thực hành Tốt nhất về Xử lý Lỗi Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Xử lý Lỗi Nâng cao trong Swift: [Bài viết trên Medium](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
