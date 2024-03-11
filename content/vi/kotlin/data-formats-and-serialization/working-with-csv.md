---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:20.867837-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB\
  \ t\xE1ch bi\u1EC7t b\u1EB1ng d\u1EA5u ph\u1EA9y) bao g\u1ED3m vi\u1EC7c \u0111\u1ECD\
  c v\xE0 vi\u1EBFt d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3n,\
  \ n\u01A1i m\u1ED7i d\xF2ng c\xF3 c\xE1c tr\u01B0\u1EDDng\u2026"
lastmod: '2024-03-11T00:14:09.903887-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB\
  \ t\xE1ch bi\u1EC7t b\u1EB1ng d\u1EA5u ph\u1EA9y) bao g\u1ED3m vi\u1EC7c \u0111\u1ECD\
  c v\xE0 vi\u1EBFt d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1ng v\u0103n b\u1EA3n,\
  \ n\u01A1i m\u1ED7i d\xF2ng c\xF3 c\xE1c tr\u01B0\u1EDDng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
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
