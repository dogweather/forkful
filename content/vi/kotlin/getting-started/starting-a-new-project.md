---
title:                "Bắt đầu một dự án mới"
aliases:
- /vi/kotlin/starting-a-new-project.md
date:                  2024-01-28T22:09:32.193236-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
