---
title:                "Sử dụng biểu thức chính quy"
aliases:
- vi/kotlin/using-regular-expressions.md
date:                  2024-01-28T22:09:33.900186-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là công cụ để tìm kiếm mẫu trong văn bản. Lập trình viên sử dụng chúng để tìm kiếm, kiểm tra, hoặc thao tác dữ liệu một cách hiệu quả.

## Làm thế nào:
Kotlin làm cho regex trở nên dễ dàng. Hãy xem một số ví dụ mã cụ thể:

```Kotlin
fun regexFind() {
    val pattern = "Kotlin".toRegex()
    val text = "Học Kotlin thật vui!"
    val matchResult = pattern.find(text)
    println(matchResult?.value) // Kết quả: Kotlin
}

fun regexReplace() {
    val regex = "\\d+".toRegex()
    val address = "123 Main Street"
    val sanitizedAddress = regex.replace(address, "###")
    println(sanitizedAddress) // Kết quả: ### Main Street
}

fun regexValidate() {
    val passwordPattern = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$".toRegex()
    val password = "Password123"
    val isPasswordValid = passwordPattern.matches(password)
    println(isPasswordValid) // Kết quả: true
}

regexFind()
regexReplace()
regexValidate()
```

## Sâu hơn
Regex đã trở thành công cụ không thể thiếu trong lập trình kể từ những năm 1950, được phát minh bởi nhà toán học Stephen Kleene. Các phương thức thay thế cho regex bao gồm các phương thức chuỗi như `contains`, `startsWith`, hoặc `split`, nhưng chúng không mạnh mẽ bằng. Regex của Kotlin được xây dựng dựa trên các lớp `Pattern` và `Matcher` của Java, mang lại hiệu suất và tiện ích robust.

## Xem thêm
- Tài liệu Kotlin về Regex: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Công cụ Kiểm tra Regex: [regex101.com](https://regex101.com/)
- Hướng dẫn Regex: [regular-expressions.info](https://www.regular-expressions.info/tutorial.html)
