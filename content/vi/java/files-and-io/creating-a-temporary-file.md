---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:12.156103-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Java, g\xF3i `java.nio.file` l\xE0\
  \ ng\u01B0\u1EDDi b\u1EA1n c\u1EE7a b\u1EA1n \u0111\u1ED1i v\u1EDBi c\xE1c t\u1EC7\
  p t\u1EA1m th\u1EDDi. H\xE3y xem \u0111o\u1EA1n m\xE3 n\xE0y."
lastmod: '2024-03-13T22:44:36.514541-06:00'
model: gpt-4-0125-preview
summary: "Trong Java, g\xF3i `java.nio.file` l\xE0 ng\u01B0\u1EDDi b\u1EA1n c\u1EE7\
  a b\u1EA1n \u0111\u1ED1i v\u1EDBi c\xE1c t\u1EC7p t\u1EA1m th\u1EDDi."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Cách thực hiện:
Trong Java, gói `java.nio.file` là người bạn của bạn đối với các tệp tạm thời. Hãy xem đoạn mã này:

```java
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Tạo một tệp tạm thời
            Path tempFile = Files.createTempFile(null, ".tmp");
            System.out.println("Tệp tạm thời được tạo tại: " + tempFile);

            // Ghi vào tệp tạm thời
            Files.writeString(tempFile, "Đây là nội dung của tệp tạm thời");

            // Đọc từ tệp tạm thời
            String content = Files.readString(tempFile);
            System.out.println("Nội dung của tệp tạm thời: " + content);

            // Xóa tệp tạm thời (tùy chọn ở đây vì nó sẽ được xóa khi JVM thoát)
            Files.delete(tempFile);
            System.out.println("Tệp tạm thời đã được xóa.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Chạy nó, và bạn sẽ nhận được cái gì đó như:

```
Tệp tạm thời được tạo tại: /tmp/user23423842348234823948.tmp
Nội dung của tệp tạm thời: Đây là nội dung của tệp tạm thời
Tệp tạm thời đã được xóa.
```

Tuyệt, đúng không?

## Sâu hơn
Tệp tạm thời là một phần của bộ công cụ của chúng ta từ lâu, từ thời kì bình minh của công nghệ máy tính. Chúng là lựa chọn tốt nhất của bạn khi bạn cần xử lý dữ liệu không cần tồn tại lâu dài.

Java hỗ trợ bạn với lớp `Files` từ Java 7, làm cho việc xử lý tệp tạm thời trở nên cực kì đơn giản. Trước đó, bạn phải xoay sở với các đối tượng `File` và hy vọng mọi thứ diễn ra tốt đẹp (nhưng đừng quay trở lại những ngày tối tăm đó, hãy ôm lấy API mới).

Phần hay về phương thức `createTempFile` là bạn có thể chỉ định thư mục và một tiền tố hoặc hậu tố tên tệp, hoặc để tất cả cho Java quyết định theo mặc định. Chỉ cần nhớ nếu bạn không xóa các tệp này một cách thủ công, chúng sẽ tồn tại cho đến khi chương trình kết thúc. Và trong một số trường hợp, đặc biệt là với các ứng dụng chạy lâu dài, bạn muốn tự mình dọn dẹp thay vì chờ đợi phút cuối cùng.

Có cách thay thế không? Chắc chắn, bạn có thể lựa chọn cách làm cũ và xử lý mọi thao tác tệp một cách thủ công, hoặc sử dụng một phương thức đặc trưng cho hệ điều hành. Tuy nhiên, cách làm của Java an toàn và dễ dàng di chuyển hơn giữa các nền tảng.

## Xem thêm
- [Tài liệu về Lớp Đường dẫn Java](https://docs.oracle.com/javase/10/docs/api/java/nio/file/Path.html)
- [Tài liệu về Lớp Tệp Java](https://docs.oracle.com/javase/10/docs/api/java/nio/file/Files.html)
- [Hướng dẫn của Oracle về I/O Tệp](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
