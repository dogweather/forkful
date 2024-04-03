---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:57.599618-07:00
description: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3\
  n t\u1EA1i trong Kotlin gi\xFAp b\u1EA1n x\xE1c nh\u1EADn li\u1EC7u m\u1ED9t folder\
  \ c\xF3 m\u1EB7t tr\u01B0\u1EDBc khi b\u1EA1n th\u1EED \u0111\u1ECDc ho\u1EB7c vi\u1EBF\
  t t\u1EC7p v\xE0o \u0111\xF3. \u0110i\u1EC1u n\xE0y gi\xFAp\u2026"
lastmod: '2024-03-13T22:44:36.620555-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i trong Kotlin gi\xFAp b\u1EA1n x\xE1c nh\u1EADn li\u1EC7u m\u1ED9t folder c\xF3\
  \ m\u1EB7t tr\u01B0\u1EDBc khi b\u1EA1n th\u1EED \u0111\u1ECDc ho\u1EB7c vi\u1EBF\
  t t\u1EC7p v\xE0o \u0111\xF3."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Làm thế nào:
Trong Kotlin, bạn thường sử dụng `java.io.File` (từ thư viện chuẩn của Java) để kiểm tra thư mục. Dưới đây là một ví dụ nhanh:

```Kotlin
import java.io.File

fun main() {
    val directoryPath = "path/to/directory"
    val directory = File(directoryPath)

    if (directory.exists() && directory.isDirectory) {
        println("Thư mục tồn tại!")
    } else {
        println("Thư mục không tồn tại.")
    }
}
```

Kết quả mẫu khi thư mục tồn tại:

```
Thư mục tồn tại!
```

Và khi nó không tồn tại:

```
Thư mục không tồn tại.
```

## Đi sâu vào vấn đề
Phương thức `exists()` trong Java đã tồn tại từ những ngày đầu của Java. Khi Kotlin xuất hiện, nó giữ vững khả năng tương tác mạnh mẽ với Java, cho phép chúng ta sử dụng trực tiếp thư viện Java. `exists()` trả về `true` nếu một tệp hoặc thư mục tồn tại, nhưng để chắc chắn rằng đó là một thư mục, không phải là tệp, chúng ta cũng kiểm tra `isDirectory`.

Bây giờ, các lựa chọn khác:

- **Gói `java.nio.file` của Kotlin**: Nó cung cấp `Files.exists(path)` và `Files.isDirectory(path)`. Những phương thức này hoạt động tương tự nhưng cung cấp nhiều điều khiển hơn đối với thuộc tính tệp.

- **Thư viện cụ thể của Kotlin**: Một số thư viện do cộng đồng điều hành mở rộng khả năng xử lý tệp của Kotlin. Chúng cung cấp các giải pháp Kotlin mang tính cách ngữ học hơn, nhưng phần lớn bên dưới, chúng thường chỉ là bọc quanh các lớp I/O của Java.

Làm việc với thư mục là sự kết hợp của việc kiểm tra sự hiện diện (nó có tồn tại không?) và loại (nó là tệp hay thư mục?). Cả hai kiểm tra đều là chìa khóa để ngăn chương trình của bạn gặp phải tình trạng hệ thống tệp không mong đợi.

## Xem thêm
- [Tài liệu API `File`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html) - Tìm hiểu về những gì bạn có thể làm với `File`.
- [Tài liệu API `Files` trong gói java.nio.file](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html) - Đối với các thao tác tệp nâng cao.
- [Tài liệu chính thức của Kotlin](https://kotlinlang.org/docs/home.html) - Tìm hiểu thêm về khả năng của Kotlin.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/kotlin) - Tìm vấn đề và giải pháp do cộng đồng cung cấp.
