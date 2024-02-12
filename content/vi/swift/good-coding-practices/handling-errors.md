---
title:                "Xử lý lỗi"
aliases:
- /vi/swift/handling-errors/
date:                  2024-01-28T22:02:43.638429-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/swift/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Xử lý lỗi trong Swift có nghĩa là dự đoán và phản ứng với những vấn đề phát sinh khi mã của bạn được chạy. Chúng ta làm điều này để kiểm soát sự hỗn loạn—giữ cho ứng dụng không bị sập và mang lại cho người dùng trải nghiệm mượt mà.

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
