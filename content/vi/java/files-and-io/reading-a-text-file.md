---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:15.432501-07:00
description: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n s\u1EBD thu n\u1EA1p n\u1ED9i dung\
  \ t\u1EEB m\u1ED9t t\u1EC7p d\u01B0\u1EDBi d\u1EA1ng m\u1ED9t chu\u1ED7i. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD ho\u1EB7\
  c ph\xE2n\u2026"
lastmod: 2024-02-19 22:04:55.676960
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n c\xF3 ngh\u0129a l\xE0\
  \ ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n s\u1EBD thu n\u1EA1p n\u1ED9i dung\
  \ t\u1EEB m\u1ED9t t\u1EC7p d\u01B0\u1EDBi d\u1EA1ng m\u1ED9t chu\u1ED7i. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 x\u1EED l\xFD ho\u1EB7\
  c ph\xE2n\u2026"
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Đọc một tệp văn bản có nghĩa là chương trình của bạn sẽ thu nạp nội dung từ một tệp dưới dạng một chuỗi. Lập trình viên làm điều này để xử lý hoặc phân tích dữ liệu nằm trong các tệp trên đĩa của họ. Đây là nhiệm vụ căn bản cho các công việc như cấu hình, phân tích dữ liệu, hoặc thậm chí chỉ để rút ra danh sách việc cần làm của bạn.

## Làm thế nào:

Đọc một tệp trong Java thật dễ dàng, đặc biệt là với `java.nio.file`. Dưới đây là một ví dụ nhanh:

```java
import java.nio.file.Files;
import java.nio.file.Path;
import java.io.IOException;
import java.util.stream.Stream;

public class FileReadExample {
    public static void main(String[] args) {
        Path filePath = Path.of("example.txt");

        try (Stream<String> lines = Files.lines(filePath)) {
            lines.forEach(System.out::println);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Chạy đoạn code này với `example.txt` chứa "Hello, file readers!" sẽ xuất ra:

```
Hello, file readers!
```

## Sâu hơn

Java đã phát triển. Trước đây, bạn phải tự quản lý stream và reader - rất nhiều đoạn mã mẫu. Gói `java.io` từng được ưa chuộng, với `FileReader` và `BufferedReader` thường được sử dụng. Sau đó, `java.nio` ra đời, cung cấp channels và buffers cho nhiều điều khiển hơn.

Bây giờ, `java.nio.file` còn cao cấp hơn. `Files` và `Paths` đơn giản hóa công việc. Ví dụ trên sử dụng `Files.lines`, stream các dòng một cách lười biếng, tốt cho các tệp lớn. Bạn cũng nhận được try-with-resources, tự động đóng stream để tránh rò rỉ.

Các phương án thay thế? `Scanner` rất tiện lợi cho việc phân tích. Apache Commons IO và Google's Guava có các tiện ích cho các nhiệm vụ phức tạp hơn, nếu bạn cần. Tuy nhiên, Java thuần thường đưa bạn đi khá xa.

Về mặt triển khai, mã hóa tệp quan trọng. `Files.lines` giả sử UTF-8 là mặc định nhưng bạn có thể chỉ định khác. Mặt khác, `BufferedReader` yêu cầu bạn phải thiết lập `Charset` trước nếu nó không phải là mặc định.

## Xem thêm

Để biết thêm thông tin thú vị, hãy tham khảo những nguồn sau:

- Lớp [`Files`](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html) trong tài liệu chính thức của Java.
- [Đọc, viết và tạo tệp](https://docs.oracle.com/javase/tutorial/essential/io/file.html) cho một hướng dẫn kỹ lưỡng.
- [Apache Commons IO](https://commons.apache.org/proper/commons-io/) cho một thư viện mạnh mẽ các tiện ích IO tệp.
