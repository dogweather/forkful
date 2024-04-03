---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:50.434107-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE0m `toLowerCase()` c\u1EE7a Kotlin\
  \ chuy\u1EC3n t\u1EA5t c\u1EA3 c\xE1c k\xFD t\u1EF1 trong m\u1ED9t chu\u1ED7i th\xE0\
  nh ch\u1EEF th\u01B0\u1EDDng m\u1ED9t c\xE1ch nhanh ch\xF3ng. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 c\xE1ch b\u1EA1n s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.583922-06:00'
model: gpt-4-0125-preview
summary: "H\xE0m `toLowerCase()` c\u1EE7a Kotlin chuy\u1EC3n t\u1EA5t c\u1EA3 c\xE1\
  c k\xFD t\u1EF1 trong m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng m\u1ED9\
  t c\xE1ch nhanh ch\xF3ng."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Cách thực hiện:
Hàm `toLowerCase()` của Kotlin chuyển tất cả các ký tự trong một chuỗi thành chữ thường một cách nhanh chóng. Dưới đây là cách bạn sử dụng nó:

```kotlin
fun main() {
    val originalString = "ThiS iS A MixED cAsE String!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString) // Kết quả: this is a mixed case string!
}
```
Gọi `lowercase()` và bạn đã hoàn tất. Chữ in hoa trong đầu vào không quan trọng; kết quả sẽ toàn chữ thường.

## Khám phá sâu hơn
Kotlin không tái tạo lại bánh xe cho việc chuyển chữ thành chữ thường. Thực ra, đây là tính năng phổ biến giữa các ngôn ngữ lập trình. Lịch sử, các hàm như `tolower()` của C đã lâu đời đối phó với việc chuyển đổi chữ hoa và thường.

Giờ đây, có hai nét mới khi chuyển chữ thường: ngữ cảnh và hiệu suất. `lowercase()` của Kotlin có thể chấp nhận một `Locale` bởi vì, bất ngờ, việc biến đổi chữ hoa không phải là phổ quát. Ví dụ, chữ 'I' có dấu và không dấu của Thổ Nhĩ Kỳ có hành vi độc đáo trong việc chuyển đổi chữ hoa và thường.

Về hiệu suất? Trong hầu hết các ứng dụng, bạn sẽ không nhận thấy. Nhưng việc xử lý văn bản quy mô lớn tiêu thụ nhiều bộ nhớ và thời gian hơn bởi vì chuỗi trong Kotlin là bất biến. Khi bạn chuyển một chuỗi sang chữ thường, bạn nhận được một chuỗi mới.

Các lập trình viên lão làng nhớ đến `.toLowerCase()` — Kotlin giờ đây ưu tiên `lowercase()` để rõ ràng hơn.

## Tham khảo
- Tài liệu về Chuỗi Kotlin: [Kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Đối với việc xử lý văn bản và thao tác trường hợp nâng cao, hãy kiểm tra API `java.lang.String`: [Oracle Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- Hiểu biết về ngữ cảnh và đặc điểm ngôn ngữ: [Oracle Locale Docs](https://docs.oracle.com/javase/tutorial/i18n/locale/)
