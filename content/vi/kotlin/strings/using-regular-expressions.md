---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:33.900186-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 c\xF4ng c\u1EE5 \u0111\
  \u1EC3 t\xECm ki\u1EBFm m\u1EABu trong v\u0103n b\u1EA3n. L\u1EADp tr\xECnh vi\xEA\
  n s\u1EED d\u1EE5ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, ki\u1EC3m tra, ho\u1EB7\
  c thao t\xE1c d\u1EEF li\u1EC7u m\u1ED9t c\xE1ch\u2026"
lastmod: '2024-03-13T22:44:36.588092-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 c\xF4ng c\u1EE5 \u0111\u1EC3\
  \ t\xECm ki\u1EBFm m\u1EABu trong v\u0103n b\u1EA3n."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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
