---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:19.009509-07:00
description: "C\xE1ch th\u1EE9c: H\xE3y in c\xE1c th\xF4ng tin ra m\xE0n h\xECnh console."
lastmod: '2024-03-13T22:44:36.605105-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y in c\xE1c th\xF4ng tin ra m\xE0n h\xECnh console."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Cách thức:
Hãy in các thông tin ra màn hình console:

```Kotlin
fun main() {
    val magicNumber = 42
    println("Con số ma thuật là $magicNumber")

    debugPrint("Con số ma thuật bình phương bằng ${magicNumber * magicNumber}")
}

fun debugPrint(message: String) {
    if (BuildConfig.DEBUG) {
        println("DEBUG: $message")
    }
}
```
Output mẫu:
```
Con số ma thuật là 42
DEBUG: Con số ma thuật bình phương bằng 1764
```
Nhanh chóng và dễ dàng, bạn có thể thấy các giá trị của mình ngay tại console.

## Tìm hiểu Sâu hơn
In ra màn hình console để gỡ lỗi là một phương pháp cũ như trái đất. Nó đơn giản, phổ biến trong tất cả các ngôn ngữ lập trình, và nó giúp hoàn thành công việc. Nhưng, nó không phức tạp, và trong các hệ thống phức tạp, quá nhiều output có thể tạo nên một bãi rác.

Các lựa chọn thay thế cho `println` trong Kotlin có thể là sử dụng các framework log như `Log4j` hoặc tiện ích `Logging` được tích hợp sẵn trong Kotlin, giúp lọc các thông điệp dựa trên cấp độ nghiêm trọng.

Một điểm tinh tế trong Kotlin, như được thấy trong hàm `debugPrint` của chúng ta, là kiểm tra xem chúng ta có đang trong một bản build gỡ lỗi không; Như vậy, chúng ta sẽ không làm rối thông tin log sản xuất với các thông điệp gỡ lỗi của mình, giữ cho các bản triển khai thực tế của chúng ta sạch sẽ và thân thiện với người dùng.

## Xem Thêm
- Để biết giới thiệu về việc log trong Kotlin, hãy truy cập tài liệu chính thức: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- Quan điểm của JetBrains về các chiến lược gỡ lỗi: [IntelliJ IDEA Debugging](https://www.jetbrains.com/help/idea/debugging-code.html)
- Nếu bạn đang sử dụng Android, hướng dẫn chính thức về việc sử dụng Logcat là tài liệu không thể thiếu: [Tài liệu Android Logcat](https://developer.android.com/studio/command-line/logcat)
