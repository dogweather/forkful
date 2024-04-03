---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:53.200525-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Kotlin, b\u1EA1n c\xF3 th\u1EC3 d\u1EC5\
  \ d\xE0ng \u0111\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n s\u1EED d\u1EE5ng h\xE0\
  m `readLines()` ho\u1EB7c kh\u1ED1i `useLines`."
lastmod: '2024-03-13T22:44:36.624436-06:00'
model: gpt-4-0125-preview
summary: "Trong Kotlin, b\u1EA1n c\xF3 th\u1EC3 d\u1EC5 d\xE0ng \u0111\u1ECDc m\u1ED9\
  t t\u1EC7p v\u0103n b\u1EA3n s\u1EED d\u1EE5ng h\xE0m `readLines()` ho\u1EB7c kh\u1ED1\
  i `useLines`."
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

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
