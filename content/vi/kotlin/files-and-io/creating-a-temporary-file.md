---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:44.437544-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch nhanh\
  \ ch\xF3ng \u0111\u1EC3 t\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi b\u1EB1ng\
  \ Kotlin."
lastmod: '2024-03-13T22:44:36.626981-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch nhanh ch\xF3ng \u0111\u1EC3 t\u1EA1\
  o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi b\u1EB1ng Kotlin."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Làm thế nào:
Dưới đây là cách nhanh chóng để tạo một tệp tạm thời bằng Kotlin:

```Kotlin
import java.io.File

fun main() {
    val tempFile = File.createTempFile("myTempFile", ".tmp")

    println("Tệp tạm thời được tạo tại: ${tempFile.absolutePath}")

    // Viết vào tệp tạm
    tempFile.writeText("Kotlin khá là tuyệt, phải không?")

    // Xóa khi thoát
    tempFile.deleteOnExit()
}
```

Kết quả sẽ là như thế này:

```
Tệp tạm thời được tạo tại: /tmp/myTempFile1234567890.tmp
```

Đường dẫn tệp tạm thời của bạn sẽ khác. Nó sẽ có một tên duy nhất nên bạn không cần phải lo lắng về việc trùng tên.

## Sâu hơn
Phương thức `File.createTempFile()` là vàng mười cho việc tạo tệp ngẫu nhiên. Nó đã có từ những ngày đầu của Java và Kotlin, là một ngôn ngữ JVM, tận dụng triệt để.

Một số sự lựa chọn khác:
- `Files.createTempFile()` từ `java.nio.file` cung cấp nhiều kiểm soát hơn, như thiết lập thuộc tính tệp.
- Các cơ sở dữ liệu hoặc bộ nhớ đệm trong bộ nhớ có thể thay thế tệp tạm thời cho một số trường hợp sử dụng (như `H2` hoặc `Redis`).

Mặc định, các tệp tạm thời được lưu trữ trong thư mục tệp tạm thời mặc định của hệ thống, nhưng bạn có thể chỉ định đường dẫn của riêng mình. Hãy nhớ dọn dẹp sau khi bạn; các tệp tạm thời không được đảm bảo sẽ bị xóa sau khi chương trình của bạn chạy. Phương thức `deleteOnExit()` đảm bảo rằng tệp được xóa khi JVM tắt, nhưng nó không phải là giải pháp chắc chắn cho các ứng dụng chạy dài hạn.

## Xem thêm
Thêm về tệp tạm thời trong Kotlin và Java:
- Tài liệu chính thức `File` của Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- Lớp `File` của Java: [https://docs.oracle.com/javase/7/docs/api/java/io/File.html](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Để hiểu sâu hơn về thuộc tính tệp: [https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html](https://docs.oracle.com/javase/tutorial/essential/io/fileAttr.html)
