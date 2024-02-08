---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:33.962022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Tìm kiếm và thay thế văn bản giống như chơi trốn tìm với chuỗi ký tự, sau đó thay người ẩn bằng người khác. Đó là một nhiệm vụ lập trình phổ biến, rất quan trọng cho các công việc như chỉnh sửa hàng loạt, làm sạch dữ liệu và tự động hóa những công việc nhàm chán.

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
