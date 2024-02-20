---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:44.437544-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi l\xE0 t\u1EA1o ra m\u1ED9\
  t t\u1EC7p \u0111\u01B0\u1EE3c d\u1EF1 \u0111\u1ECBnh s\u1ED1ng ng\u1EAFn h\u1EA1\
  n tr\xEAn h\u1EC7 th\u1ED1ng t\u1EC7p c\u1EE7a b\u1EA1n, th\u01B0\u1EDDng \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng cho nh\u1EEFng vi\u1EC7c nh\u01B0 d\u1EEF li\u1EC7\
  u trung gian.\u2026"
lastmod: 2024-02-19 22:04:55.798975
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi l\xE0 t\u1EA1o ra m\u1ED9\
  t t\u1EC7p \u0111\u01B0\u1EE3c d\u1EF1 \u0111\u1ECBnh s\u1ED1ng ng\u1EAFn h\u1EA1\
  n tr\xEAn h\u1EC7 th\u1ED1ng t\u1EC7p c\u1EE7a b\u1EA1n, th\u01B0\u1EDDng \u0111\
  \u01B0\u1EE3c s\u1EED d\u1EE5ng cho nh\u1EEFng vi\u1EC7c nh\u01B0 d\u1EEF li\u1EC7\
  u trung gian.\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tạo một tệp tạm thời là tạo ra một tệp được dự định sống ngắn hạn trên hệ thống tệp của bạn, thường được sử dụng cho những việc như dữ liệu trung gian. Các lập trình viên thực hiện điều này chủ yếu bởi vì nó có thể giúp quản lý không gian, giảm xung đột và tăng cường bảo mật trong quá trình chạy.

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
