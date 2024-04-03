---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:33.962022-07:00
description: "L\xE0m th\u1EBF n\xE0o: Kotlin \u0111\u01A1n gi\u1EA3n h\xF3a vi\u1EC7\
  c thao t\xE1c v\u1EDBi v\u0103n b\u1EA3n th\xF4ng qua th\u01B0 vi\u1EC7n ti\xEA\
  u chu\u1EA9n c\u1EE7a n\xF3. D\u01B0\u1EDBi \u0111\xE2y, xem c\xE1ch b\u1EA1n s\u1EED\
  \ d\u1EE5ng `replace` \u0111\u1EC3 \u0111\u1ED5i t\u1EEB."
lastmod: '2024-03-13T22:44:36.581231-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0111\u01A1n gi\u1EA3n h\xF3a vi\u1EC7c thao t\xE1c v\u1EDBi v\u0103\
  n b\u1EA3n th\xF4ng qua th\u01B0 vi\u1EC7n ti\xEAu chu\u1EA9n c\u1EE7a n\xF3."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Làm thế nào:
Kotlin đơn giản hóa việc thao tác với văn bản thông qua thư viện tiêu chuẩn của nó. Dưới đây, xem cách bạn sử dụng `replace` để đổi từ.

```kotlin
fun main() {
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = originalText.replace("pragmatic", "cool")

    println(newText) // Kết quả: Kotlin is fun, Kotlin is cool!
}
```

Đối với các mẫu regex:

```kotlin
fun main() {
    val regex = "Kotlin".toRegex()
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = regex.replace(originalText, "Java")

    println(newText) // Kết quả: Java is fun, Java is pragmatic!
}
```

## Tìm hiểu Sâu
Việc viết lại văn bản cũ như in ấn, nhưng trong lập trình, nó phát triển mạnh với các bộ xử lý văn bản đầu tiên. Có phương án thay thế không? Chắc chắn rồi – chức năng tìm và thay thế trong trình soạn thảo, công cụ dòng lệnh như `sed`. Cụ thể trong Kotlin, bạn có cả phương pháp chuỗi và regex để sử dụng.

`replace` là đơn giản cho văn bản đơn giản; `Regex` cung cấp cho bạn một công cụ đa năng cho các mẫu. Regexes mạnh mẽ nhưng khó khăn hơn - chúng sử dụng cú pháp đặc biệt để khớp mẫu. Hãy nghĩ về regex như chơi "Tìm Waldo", nhưng bạn là người đặt ra quy tắc về những gì Waldo mặc.

Có gì phải lưu ý khi thực hiện? Nhớ rằng, `String` của Kotlin là bất biến. Các phương thức thay đổi văn bản trả lại chuỗi mới; chúng không thay đổi chuỗi gốc.

## Xem Thêm
- Tài liệu Kotlin về `replace`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Regex trong Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- `sed` xưa cũ: https://www.gnu.org/software/sed/manual/sed.html
