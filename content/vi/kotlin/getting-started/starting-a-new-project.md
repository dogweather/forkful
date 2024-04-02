---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:32.193236-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp c\u1EA5u tr\xFAc ban \u0111\u1EA7u v\xE0 c\xE1c t\u1EC7\
  p tin quan tr\u1ECDng m\xE0 b\u1EA1n s\u1EBD c\u1EA7n cho \u1EE9ng d\u1EE5ng c\u1EE7\
  a m\xECnh. L\u1EADp tr\xECnh vi\xEAn b\u1EAFt \u0111\u1EA7u d\u1EF1 \xE1n\u2026"
lastmod: '2024-03-13T22:44:36.602591-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi c\xF3 ngh\u0129a\
  \ l\xE0 thi\u1EBFt l\u1EADp c\u1EA5u tr\xFAc ban \u0111\u1EA7u v\xE0 c\xE1c t\u1EC7\
  p tin quan tr\u1ECDng m\xE0 b\u1EA1n s\u1EBD c\u1EA7n cho \u1EE9ng d\u1EE5ng c\u1EE7\
  a m\xECnh. L\u1EADp tr\xECnh vi\xEAn b\u1EAFt \u0111\u1EA7u d\u1EF1 \xE1n\u2026"
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Cái gì và Tại sao?

Bắt đầu một dự án mới có nghĩa là thiết lập cấu trúc ban đầu và các tệp tin quan trọng mà bạn sẽ cần cho ứng dụng của mình. Lập trình viên bắt đầu dự án mới để khởi động phát triển với một bảng là sạch, được tùy chỉnh theo mục tiêu của họ và công nghệ họ dự định sử dụng.

## Làm thế nào:

Hãy bắt đầu với một dự án Kotlin sử dụng IntelliJ IDEA – một môi trường phát triển tích hợp phổ biến cho Kotlin.

1. Mở IntelliJ IDEA.
2. Chọn `File > New > Project`.
3. Chọn `Kotlin` ở thanh bên trái.
4. Chỉ định SDK của dự án bạn (thông thường, IntelliJ sẽ chọn phiên bản mới nhất mà bạn đã cài đặt).
5. Chọn một mẫu dự án hoặc giữ nguyên mẫu mặc định.
6. Đặt tên cho dự án và chọn vị trí của nó.
7. Nhấn `Finish`.

Bùm, bạn đã có một dự án Kotlin mới. Thư mục điển hình của bạn sẽ trông như sau sau khi tạo:

```plaintext
tênDựÁn
|-- .idea
|-- src
     |-- main.kt
|-- build.gradle
```

Và `main.kt` của bạn có thể bắt đầu đơn giản như thế này:

```kotlin
fun main() {
    println("Sẵn sàng, chuẩn bị, chạy Kotlin!")
}
```

Khi chạy `main.kt`, bạn sẽ thấy:

```plaintext
Sẵn sàng, chuẩn bị, chạy Kotlin!
```

## Đi sâu vào

Kotlin là một ngôn ngữ hiện đại chạy trên JVM (Java Virtual Machine), được thiết kế để ngắn gọn và an toàn. Nó được tạo ra bởi JetBrains và đã trở nên phổ biến, đặc biệt là phát triển ứng dụng Android, kể từ khi Google thông báo hỗ trợ chính thức vào năm 2017.

Trước khi nhảy vào một dự án Kotlin mới, hiểu vì sao bạn chọn Kotlin:
- Cú pháp ngắn gọn: Giảm thiểu mã rườm rà.
- Tương thích với Java: Tích hợp mượt mà với mã và thư viện Java.
- Ép kiểu thông minh: Ít cần ép kiểu rõ ràng.
- An toàn với null: Hệ thống tích hợp để tránh ngoại lệ con trỏ null.

Các phương án thay thế để bắt đầu một dự án Kotlin mới với IntelliJ IDEA:
- Dòng lệnh: Tạo tệp thủ công, biên dịch với `kotlinc`, và chạy bằng lệnh `kotlin`.
- Các IDE khác: Sử dụng Android Studio cho phát triển Android hoặc Eclipse với plugin Kotlin.

Khi bạn bắt đầu một dự án mới trong IntelliJ IDEA, nó tự động thiết lập cấu hình Gradle cần thiết. Gradle là một hệ thống tự động hóa xây dựng quản lý các phụ thuộc, xây dựng và kiểm tra cho dự án của bạn. Cài đặt này cho phép bạn nhập các thư viện, xác định các phụ thuộc mô-đun và tạo điều kiện cho việc xây dựng dễ dàng.

## Xem Thêm

Muốn tìm hiểu ngoài những kiến thức cơ bản? Dưới đây là những nơi bạn nên đến tiếp theo:

- Tài liệu chính thức của Kotlin: [Tài liệu Kotlin](https://kotlinlang.org/docs/home.html)
- Cơ bản về Gradle: [Hướng dẫn sử dụng Gradle](https://docs.gradle.org/current/userguide/userguide.html)

Nhớ rằng, cách tốt nhất để học là bằng cách làm. Bắt đầu với 'Hello World' và tiếp tục xây dựng từ đó. Chúc bạn lập trình vui vẻ!
