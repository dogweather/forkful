---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:04:53.200525-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Đọc một tệp văn bản có nghĩa là kéo dữ liệu từ một tệp vào chương trình của bạn, thường là từng dòng một. Lập trình viên làm điều này để xử lý hoặc phân tích dữ liệu được lưu trữ bên ngoài.

## Cách thực hiện:

Trong Kotlin, bạn có thể dễ dàng đọc một tệp văn bản sử dụng hàm `readLines()` hoặc khối `useLines`.

```Kotlin
import java.io.File

fun main() {
    // Đọc tất cả các dòng cùng một lúc
    val lines = File("example.txt").readLines()
    lines.forEach { line ->
        println(line)
    }

    // Hiệu quả hơn cho các tệp lớn
    File("example.txt").useLines { lines ->
        lines.forEach { line ->
            println(line)
        }
    }
}
```

Kết quả mẫu (giả sử `example.txt` chứa hai dòng với "Hello" và "World"):

```
Hello
World
```

## Tìm hiểu sâu hơn

Trong lịch sử, đọc tệp trong Java có thể dài dòng và cồng kềnh. Với Kotlin, thư viện tiêu chuẩn cung cấp các phần mở rộng tiện ích để làm cho việc đọc tệp trở nên đơn giản hơn.

Có các phương án thay thế cho việc đọc tệp trong Kotlin:
1. `readText()` đọc toàn bộ nội dung của tệp vào một `String`.
2. `bufferedReader()` cung cấp một `BufferedReader` cho phép bạn xử lý các trường hợp sử dụng phức tạp hơn như đọc các tệp lớn mà không tiêu thụ quá nhiều bộ nhớ.

Về mặt triển khai, khi bạn sử dụng `useLines`, nó tự động đóng tệp sau khi thực thi, ngăn chặn rò rỉ bộ nhớ tiềm ẩn. Đó là một cách tiếp cận hàm mà Kotlin khuyến khích khi quản lý tài nguyên hiệu quả.

## Xem thêm

- Tài liệu Kotlin về đọc tệp: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Tài liệu `BufferedReader` cho các trường hợp phức tạp hơn: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
