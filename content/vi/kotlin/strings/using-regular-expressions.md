---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:33.900186-07:00
description: "L\xE0m th\u1EBF n\xE0o: Kotlin l\xE0m cho regex tr\u1EDF n\xEAn d\u1EC5\
  \ d\xE0ng. H\xE3y xem m\u1ED9t s\u1ED1 v\xED d\u1EE5 m\xE3 c\u1EE5 th\u1EC3."
lastmod: '2024-03-13T22:44:36.588092-06:00'
model: gpt-4-0125-preview
summary: "Kotlin l\xE0m cho regex tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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
