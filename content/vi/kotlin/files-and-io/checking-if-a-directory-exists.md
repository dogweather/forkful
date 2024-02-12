---
title:                "Kiểm tra xem thư mục có tồn tại không"
aliases:
- vi/kotlin/checking-if-a-directory-exists.md
date:                  2024-01-28T21:56:57.599618-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc kiểm tra xem một thư mục có tồn tại trong Kotlin giúp bạn xác nhận liệu một folder có mặt trước khi bạn thử đọc hoặc viết tệp vào đó. Điều này giúp tránh lỗi như `FileNotFoundException`, và giúp chương trình của bạn đưa ra quyết định thông minh, như tạo thư mục nếu nó không tồn tại.

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
