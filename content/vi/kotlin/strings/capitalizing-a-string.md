---
title:                "Viết hoa một chuỗi"
aliases: - /vi/kotlin/capitalizing-a-string.md
date:                  2024-01-28T21:55:56.190929-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Việc viết hoa một chuỗi nghĩa là chuyển chữ cái đầu tiên của mỗi từ thành chữ in hoa. Các lập trình viên thực hiện việc này để định dạng văn bản, đảm bảo tên, tiêu đề hoặc các phần tử giao diện người dùng trông gọn gàng và tiêu chuẩn.

## Làm Thế Nào:

Trong Kotlin, bạn có thể dễ dàng viết hoa các chuỗi. Dưới đây là một ví dụ nhanh:

```kotlin
fun main() {
    val text = "lập trình kotlin"
    val capitalizedText = text.split(" ").joinToString(" ") { it.capitalize() }
    println(capitalizedText)
}
```

Kết Quả Mẫu:
```
Lập Trình Kotlin
```
Để viết hoa chỉ chữ cái đầu tiên của một câu:

```kotlin
fun main() {
    val sentence = "xin chào, những người đam mê kotlin!"
    val capitalizedSentence = sentence.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizedSentence)
}

```

Kết Quả Mẫu:
```
Xin chào, những người đam mê kotlin!
```

Lưu ý rằng `capitalize()` đã bị loại bỏ. Sử dụng `replaceFirstChar { it.titlecase() }` để tương thích tốt hơn trong tương lai.

## Sâu Hơn

Phương thức viết hoa trong Kotlin đã thay đổi. `capitalize()` từng được sử dụng rộng rãi nhưng đã bị loại bỏ ủng hộ `replaceFirstChar { it.titlecase() }`. Sự thay đổi này làm cho mã lệnh rõ ràng hơn về những gì đang xảy ra - không chỉ viết hoa mà còn thay thế ký tự đầu tiên bằng tương đương titlecase của nó.

Tại sao lại viết hoa chuỗi? Thường là liên quan đến giao diện người dùng. Hãy nghĩ đến tiêu đề sách, tên, hoặc bất kỳ danh sách nào mà bạn cần sự nhất quán. Nó giúp tăng tính dễ đọc và tính thẩm mỹ.

Các phương án thay thế cho việc viết hoa bao gồm:
- `.toLowerCase()`: Để chuyển thành chữ thường.
- `.toUpperCase()`: Để chuyển tất cả thành chữ in hoa.
- CSS trong phát triển web: đôi khi văn bản được viết hoa ở phía frontend.

Bên dưới lớp vỏ, các hàm viết hoa tương tác với các ký tự Unicode. Các ký tự có phiên bản in hoa cụ thể. Không chỉ là thay thế 'a' bằng 'A', mà còn là hiểu các quy tắc cụ thể của ngôn ngữ.

Đừng quên về địa phương hóa. Ở Thổ Nhĩ Kỳ, chẳng hạn, 'i' được viết hoa thành 'İ', không phải 'I'. Vì vậy, thực hiện việc này mà không quan tâm đến địa phương có thể gây lỗi trong các ứng dụng đa ngôn ngữ.

## Xem Thêm:

- Tài liệu Kotlin về `replaceFirstChar`: [Kotlin replaceFirstChar](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- Các quy tắc viết hoa của Unicode: [Hướng dẫn Viết Hoa Unicode](http://unicode.org/versions/Unicode9.0.0/ch03.pdf#G33992)
- Viết hoa theo địa phương cụ thể: [Viết Hoa Theo Địa Phương Cụ Thể](https://garygregory.wordpress.com/2015/11/03/java-lowercase-conversion-turkey/)
