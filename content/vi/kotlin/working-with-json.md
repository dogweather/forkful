---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:36.530463-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?

JSON (JavaScript Object Notation) là một định dạng để cấu trúc dữ liệu, được sử dụng cho việc lưu trữ và truyền dữ liệu. Lập trình viên sử dụng nó vì nó nhẹ, dễ đọc, và dễ dàng được các ngôn ngữ khác nhau phân tích cú pháp, bao gồm Kotlin.

## Làm thế nào:

Để làm việc với JSON trong Kotlin, bạn có thể sử dụng thư viện `kotlinx.serialization`. Dưới đây là một ví dụ đơn giản về việc tuần tự hóa và giải tuần tự hóa một lớp dữ liệu.

```Kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.encodeToString
import kotlinx.serialization.decodeFromString

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json { prettyPrint = true }
    val userData = User("John Doe", 30)
    
    // Tuần tự hóa thành JSON
    val jsonString = json.encodeToString(userData)
    println(jsonString)
    
    // Giải tuần tự hóa từ JSON
    val userObj = json.decodeFromString<User>(jsonString)
    println(userObj)
}
```

Đầu ra mẫu:

```
{
    "name": "John Doe",
    "age": 30
}
User(name=John Doe, age=30)
```

## Tìm hiểu sâu hơn

Cú pháp đơn giản của JSON có nguồn gốc từ JavaScript, nhưng bây giờ nó độc lập với ngôn ngữ. Các phương thức khác như XML thì dài dòng hơn. Khi làm việc với JSON trong Kotlin, thư viện `kotlinx.serialization` giải quyết phần nặng nề, tự động chuyển đổi đối tượng Kotlin thành JSON và ngược lại với các chú thích. Nó hỗ trợ các kiểu dữ liệu phức tạp và xử lý các trường hợp ngoại lệ, nhưng việc phân tích cú pháp JSON một cách thủ công cũng là một lựa chọn nếu bạn cần kiểm soát chặt chẽ hơn.

## Xem thêm

- Hướng dẫn Tuần tự hóa Kotlin: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- Giới thiệu về JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
