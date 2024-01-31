---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:13:20.528705-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết một tập tin văn bản trong lập trình liên quan đến việc tạo và lưu dữ liệu vào một định dạng tập tin có thể đọc được, như .txt. Lập trình viên làm điều này để lưu trữ dữ liệu, cấu hình hệ thống, ghi nhận thông tin, hoặc xuất nội dung dễ đọc cho con người.

## Làm thế nào:

Hãy viết "Hello, file!" vào tập tin "greeting.txt".

```Kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, file!"
    File("greeting.txt").writeText(textToWrite)
}
```

Sau khi chạy:
```
Hello, file! (trong greeting.txt)
```

Nếu chúng ta cần thêm văn bản thay vì ghi đè thì sao?

```Kotlin
fun appendTextToFile(filename: String, text: String) {
    File(filename).appendText("\n$text")
}

fun main() {
    appendTextToFile("greeting.txt", "Another line!")
}
```

Kết quả trong `greeting.txt`:
```
Hello, file!
Another line!
```

## Tìm hiểu sâu hơn

Lịch sử, tập tin văn bản đã là một yếu tố quan trọng trong việc cấu hình và ghi nhật ký trong các hệ thống phần mềm. Mặc dù các công cụ và định dạng (như XML, JSON) đã phát triển, tập tin văn bản vẫn là một phương pháp đơn giản, có thể truy cập mọi lúc để tương tác với dữ liệu.

Các phương pháp thay thế `java.io.File` bao gồm `java.nio.file.Files` và `java.io.FileWriter`, cung cấp nhiều quyền kiểm soát và hiệu quả hơn cho các tập tin lớn hơn hoặc các hoạt động phức tạp hơn.

Chi tiết thực hiện quan trọng:
- **Mã hóa**: Theo mặc định, `writeText` sử dụng mã hóa UTF-8. Đối với mã hóa khác, sử dụng `writeText(textToWrite, Charsets.ISO_8859_1)` hoặc tương tự.
- **Đệm**: Khi làm việc với các tập tin lớn, hãy nhớ sử dụng đệm. Bọc writer của bạn trong một `BufferedWriter` để có hiệu suất tốt hơn.
- **Xử lý Ngoại lệ**: Hãy cảnh giác với các `IOException` tiềm ẩn và xử lý chúng một cách phù hợp.

## Xem thêm

- Tài liệu chính thức của Kotlin về File IO: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Gói `java.nio.file` cho I/O tập tin hiện đại: [Java Docs](https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html)
- Tìm hiểu về `BufferedWriter` cho viết hiệu quả: [Java BufferedWriter](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedWriter.html)
