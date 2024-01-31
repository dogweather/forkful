---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:11:20.867837-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với CSV (Comma-Separated Values - Giá trị tách biệt bằng dấu phẩy) bao gồm việc đọc và viết dữ liệu dưới dạng văn bản, nơi mỗi dòng có các trường tách biệt nhau bởi dấu phẩy. Lập trình viên sử dụng nó bởi vì đó là cách đơn giản, được hỗ trợ rộng rãi để trao đổi dữ liệu có cấu trúc giữa các hệ thống và ứng dụng.

## Làm thế nào:

Để làm việc với CSV trong Kotlin, bạn có thể sử dụng thư viện cốt lõi hoặc các thư viện của bên thứ ba như Kotlinx.serialization hay Apache Commons CSV. Ở đây, tôi sẽ chỉ cho bạn cách thực hiện nhập/xuất cơ bản mà không cần thư viện bên ngoài.

```kotlin
import java.io.File

fun main() {
    // Ghi vào CSV
    val outputFile = File("data.csv")
    outputFile.printWriter().use { out ->
        out.println("id,name,age")
        out.println("1,John Doe,30")
        out.println("2,Jane Smith,25")
    }

    // Đọc từ CSV
    File("data.csv").forEachLine { line ->
        val (id, name, age) = line.split(',')
        println("ID: $id, Tên: $name, Tuổi: $age")
    }
}
```

Kết quả:
```text
ID: 1, Tên: John Doe, Tuổi: 30
ID: 2, Tên: Jane Smith, Tuổi: 25
```

## Nghiên cứu sâu

CSV có nguồn gốc từ những ngày đầu của máy tính khi bộ nhớ bị hạn chế và các định dạng trao đổi dữ liệu cần phải đơn giản. Mặc dù các lựa chọn thay thế như JSON và XML đã xuất hiện, CSV vẫn phổ biến vì tính dễ sử dụng, khả năng tương thích, và vì nó dễ đọc với con người.

Xử lý CSV đúng cách có thể phức tạp hơn do các trường hợp ngoại lệ (như dấu phẩy trong dữ liệu, các trường nhiều dòng, v.v.). Các thư viện như Apache Commons CSV và Kotlinx.serialization xử lý những trường hợp này và cung cấp thêm các chức năng khác.

## Xem thêm

- [RFC 4180](https://tools.ietf.org/html/rfc4180): Định dạng chung và kiểu MIME cho các tệp CSV.
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/): Thư viện Java dùng để xử lý tệp CSV có thể được sử dụng trong Kotlin.
- [Kotlinx.serialization CSV](https://github.com/Kotlin/kotlinx.serialization): Thư viện Kotlin giúp việc serialization đến và từ định dạng CSV trở nên đơn giản hơn.
