---
title:                "Viết một tệp văn bản"
aliases: - /vi/java/writing-a-text-file.md
date:                  2024-01-28T22:12:33.710991-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết một tệp văn bản trong Java có nghĩa là lưu trữ dữ liệu văn bản vào một tệp trên đĩa. Các nhà phát triển thực hiện việc này cho các tác vụ như ghi nhật ký, cấu hình, hoặc xuất dữ liệu dễ đọc cho con người.

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
