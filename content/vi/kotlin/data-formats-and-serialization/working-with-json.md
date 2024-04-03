---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:36.530463-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON\
  \ trong Kotlin, b\u1EA1n c\xF3 th\u1EC3 s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `kotlinx.serialization`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3\
  n v\u1EC1 vi\u1EC7c tu\u1EA7n t\u1EF1 h\xF3a v\xE0\u2026"
lastmod: '2024-03-13T22:44:36.629468-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi JSON trong Kotlin, b\u1EA1n c\xF3\
  \ th\u1EC3 s\u1EED d\u1EE5ng th\u01B0 vi\u1EC7n `kotlinx.serialization`."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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
