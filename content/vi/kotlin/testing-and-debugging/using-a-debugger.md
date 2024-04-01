---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:13.471868-07:00
description: "Vi\u1EC7c l\u1EB7n s\xE2u v\xE0o debugger l\xE0 \u0111\u1EC3 b\u01B0\
  \u1EDBc qua t\u1EEBng d\xF2ng code, quan s\xE1t c\xE1ch th\u1EE9c ho\u1EA1t \u0111\
  \u1ED9ng v\xE0 b\u1EAFt g\u1EB7p nh\u1EEFng l\u1ED7i kh\xF3 ch\u1ECBu \u0111ang\
  \ qu\u1EADy ph\xE1. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.607628-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\u1EB7n s\xE2u v\xE0o debugger l\xE0 \u0111\u1EC3 b\u01B0\u1EDB\
  c qua t\u1EEBng d\xF2ng code, quan s\xE1t c\xE1ch th\u1EE9c ho\u1EA1t \u0111\u1ED9\
  ng v\xE0 b\u1EAFt g\u1EB7p nh\u1EEFng l\u1ED7i kh\xF3 ch\u1ECBu \u0111ang qu\u1EAD\
  y ph\xE1. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

## Cách thực hiện:
Dưới đây là một vài ví dụ về việc debug trong Kotlin với IntelliJ IDEA - Sherlock Holmes của các IDE:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Đoán số: ")
        guess = readLine()?.toIntOrNull() ?: continue // Bỏ qua nhập liệu xấu

        // Đặt một breakpoint ở đây để theo dõi 'guess' trong quá trình hoạt động
        if (guess < mysteryNumber) {
            println("Quá thấp!")
        } else if (guess > mysteryNumber) {
            println("Quá cao!")
        }
    }

    println("Bạn đã đoán đúng! Số bí ẩn là $mysteryNumber")
}
```

Kết quả từ Debugger:
```
Đoán số: 
10
Quá thấp!
Đoán số: 
50
Quá cao!
Đoán số: 
42
Bạn đã đoán đúng! Số bí ẩn là 42
```

## Sâu hơn nữa
Debugger đã có mặt trong ngành từ những năm '50. Lúc bấy giờ, chúng khá là sơ khai, và việc debug có thể liên quan nhiều đến phần cứng hơn là phần mềm. Ngày nay, một debugger như cái trong IntelliJ IDEA cho phép chúng ta đặt breakpoint, bước qua từng dòng code một cách chi tiết, và kiểm tra trạng thái của các biến một cách thoải mái.

Mặc dù debugger của IntelliJ rất tiện lợi cho Kotlin, nó không phải là cái duy nhất. Có một loạt các phương án khác như Logcat cho phát triển Android, hoặc các công cụ dòng lệnh như jdb cho những người thích giản dị. Phép thuật đằng sau hậu trường ở đây chủ yếu liên quan đến JVM Tool Interface (JVMTI), điều này cho phép debugger tương tác với Máy Ảo Java, giữ cho các nhà phát triển Kotlin được thông suốt.

## Xem thêm
- Tài liệu Debugger của IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
