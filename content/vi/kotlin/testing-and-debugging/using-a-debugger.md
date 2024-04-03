---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:13.471868-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t v\xE0i v\xED d\u1EE5 v\u1EC1 vi\u1EC7c debug trong Kotlin v\u1EDBi IntelliJ IDEA\
  \ - Sherlock Holmes c\u1EE7a c\xE1c IDE."
lastmod: '2024-03-13T22:44:36.607628-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xE0i v\xED d\u1EE5 v\u1EC1 vi\u1EC7\
  c debug trong Kotlin v\u1EDBi IntelliJ IDEA - Sherlock Holmes c\u1EE7a c\xE1c IDE."
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
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
