---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:20.528705-07:00
description: "Vi\u1EBFt m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n trong l\u1EADp tr\xEC\
  nh li\xEAn quan \u0111\u1EBFn vi\u1EC7c t\u1EA1o v\xE0 l\u01B0u d\u1EEF li\u1EC7\
  u v\xE0o m\u1ED9t \u0111\u1ECBnh d\u1EA1ng t\u1EADp tin c\xF3 th\u1EC3 \u0111\u1ECD\
  c \u0111\u01B0\u1EE3c, nh\u01B0 .txt. L\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
lastmod: '2024-03-11T00:14:09.898513-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EBFt m\u1ED9t t\u1EADp tin v\u0103n b\u1EA3n trong l\u1EADp tr\xEC\
  nh li\xEAn quan \u0111\u1EBFn vi\u1EC7c t\u1EA1o v\xE0 l\u01B0u d\u1EEF li\u1EC7\
  u v\xE0o m\u1ED9t \u0111\u1ECBnh d\u1EA1ng t\u1EADp tin c\xF3 th\u1EC3 \u0111\u1ECD\
  c \u0111\u01B0\u1EE3c, nh\u01B0 .txt. L\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
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
