---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:33.710991-07:00
description: "L\xE0m th\u1EBF n\xE0o: V\u1EDBi g\xF3i `java.nio.file` c\u1EE7a Java,\
  \ vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n r\u1EA5t \u0111\u01A1n gi\u1EA3\
  n. H\xE3y xem `Files.write()` \u0111\u1EC3 l\u01B0u nhanh."
lastmod: '2024-03-13T22:44:36.513299-06:00'
model: gpt-4-0125-preview
summary: "V\u1EDBi g\xF3i `java.nio.file` c\u1EE7a Java, vi\u1EBFt v\xE0o m\u1ED9\
  t t\u1EC7p v\u0103n b\u1EA3n r\u1EA5t \u0111\u01A1n gi\u1EA3n."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

## Làm thế nào:
Với gói `java.nio.file` của Java, viết vào một tệp văn bản rất đơn giản. Hãy xem `Files.write()` để lưu nhanh:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class WriteTextFileExample {
    public static void main(String[] args) {
        List<String> lines = List.of("Dòng 1", "Dòng 2", "Dòng 3");
        Path file = Path.of("example.txt");

        try {
            Files.write(file, lines);
            System.out.println("Viết vào tệp thành công.");
        } catch (IOException e) {
            System.err.println("Ối! Đã xảy ra lỗi: " + e.getMessage());
        }
    }
}
```

Kết quả:
```
Viết vào tệp thành công.
```

## Tìm hiểu sâu hơn
Trong quá khứ, I/O của Java chủ yếu liên quan đến `FileWriter` và `BufferedWriter`. Bây giờ, gói NIO (`java.nio.file`) là lựa chọn hàng đầu. `Files.write()` rất tiện lợi — xử lý việc tạo, mở và viết trong một lần. Phương án thay thế? `FileOutputStream` cho kiểm soát cấp độ byte. Phía sau, `Files.write()` sử dụng một `BufferedWriter` và `Charset` để mã hóa văn bản thành byte.

## Xem thêm
Đào sâu hơn vào I/O tệp với những liên kết sau:

- Tài liệu chính thức của `java.nio.file.Files`: https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Hướng dẫn I/O tệp của Oracle: https://docs.oracle.com/javase/tutorial/essential/io/
- Đối với một cách tiếp cận hướng byte, khám phá `FileOutputStream`: https://docs.oracle.com/javase/8/docs/api/java/io/FileOutputStream.html
