---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:56.679602-07:00
description: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1\
  i kh\xF4ng c\xF3 ngh\u0129a l\xE0 x\xE1c minh th\u01B0 m\u1EE5c \u0111\xF3 c\xF3\
  \ m\u1EB7t tr\u01B0\u1EDBc khi b\u1EA1n c\u1ED1 g\u1EAFng \u0111\u1ECDc ho\u1EB7\
  c ghi c\xE1c t\u1EC7p v\xE0o trong \u0111\xF3. L\u1EADp tr\xECnh vi\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.508186-06:00'
model: gpt-4-0125-preview
summary: "Ki\u1EC3m tra xem m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4\
  ng c\xF3 ngh\u0129a l\xE0 x\xE1c minh th\u01B0 m\u1EE5c \u0111\xF3 c\xF3 m\u1EB7\
  t tr\u01B0\u1EDBc khi b\u1EA1n c\u1ED1 g\u1EAFng \u0111\u1ECDc ho\u1EB7c ghi c\xE1\
  c t\u1EC7p v\xE0o trong \u0111\xF3. L\u1EADp tr\xECnh vi\xEAn\u2026"
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Kiểm tra xem một thư mục có tồn tại không có nghĩa là xác minh thư mục đó có mặt trước khi bạn cố gắng đọc hoặc ghi các tệp vào trong đó. Lập trình viên làm điều này để tránh lỗi, như là cố gắng lưu một tệp vào nơi không có chỗ để đặt nó.

## Cách thực hiện:
Dưới đây là cách bạn kiểm tra xem một thư mục có tồn tại với `java.nio.file` không:

```java
import java.nio.file.Files;
import java.nio.file.Path;

public class DirectoryCheck {

    public static void main(String[] args) {
        Path directoryPath = Path.of("/path/to/directory");

        // Kiểm tra xem thư mục có tồn tại không
        boolean directoryExists = Files.exists(directoryPath);

        // In kết quả
        System.out.println("Thư mục có tồn tại? " + directoryExists);
    }
}
```

Nếu bạn chạy điều này, bảng điều khiển của bạn sẽ đơn giản hiển thị:

```
Thư mục có tồn tại? true // hoặc false
```

Cứ thử xem.

## Đào Sâu Hơn
Ngày xưa, mọi người sử dụng phương thức `java.io.File.exists()`. Nhưng `java.nio.file.Files.exists(Path)` bây giờ mới là ngôi sao sáng vì nó linh hoạt hơn. Bạn cũng có thể kiểm tra các thuộc tính tệp với cùng một API.

Nhưng đợi đã, còn nữa. Phương thức `Files.exists` không phải là không thể xuyên thủng—có một điều kiện đua. Điều gì xảy ra nếu có điều gì đó xảy ra với thư mục ngay sau khi bạn kiểm tra? Bùm, hoạt động của bạn thất bại. Để giảm thiểu điều này, hãy sử dụng `Files.exists` một cách tiết kiệm và xử lý ngoại lệ một cách đúng đắn khi thực hiện các hoạt động tệp thực sự.

Thay vào đó, bạn có thể đơn giản thử thực hiện hoạt động tệp và bắt lỗi `NoSuchFileException` có thể xảy ra. Điều này được biết đến là "dễ dàng xin lỗi hơn là xin phép" (EAFP) so với "nhìn trước khi nhảy" (LBYL), đó là những gì `Files.exists()` đang làm.

## Xem Thêm
- [Files.exists()](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#exists(java.nio.file.Path,java.nio.file.LinkOption...))
- [File I/O in Java](https://docs.oracle.com/javase/tutorial/essential/io/)
- Một bài viết hay về EAFP so với LBYL: [Nguyên tắc EAFP](https://devblogs.microsoft.com/python/idiomatic-python-eafp-versus-lbyl/)
