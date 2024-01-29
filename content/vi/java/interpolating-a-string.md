---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:29.822816-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Nội suy chuỗi cho phép bạn chèn trực tiếp các biến vào trong chuỗi. Điều này làm cho mã lệnh trở nên gọn gàng và dễ đọc hơn bằng cách tránh việc nối chuỗi cồng kềnh.

## Cách thực hiện:
Java giới thiệu `String.format()` cho việc nội suy:

```java
public class StringInterpolationExample {
  public static void main(String[] args) {
    String user = "Alice";
    int points = 1337;
    String greeting = String.format("Xin chào, %s! Bạn có %d điểm.", user, points);
    System.out.println(greeting);
  }
}
```
Kết quả mẫu:
```
Xin chào, Alice! Bạn có 1337 điểm.
```

Đối với nội suy hiện đại hơn kể từ Java 15, chúng ta sử dụng khối văn bản và `formatted()`:

```java
public class ModernStringInterpolationExample {
  public static void main(String[] args) {
    String user = "Bob";
    double accountBalance = 1234.56;
    String message = """
      Kính gửi %s,
      Số dư hiện tại của bạn là $%.2f.
      """.formatted(user, accountBalance);
    System.out.println(message);
  }
}
```
Kết quả mẫu:
```
Kính gửi Bob,
Số dư hiện tại của bạn là $1234.56.
```

## Đi sâu hơn
Trước khi có nội suy, Java phụ thuộc vào việc nối chuỗi: `String greeting = "Xin chào, " + user + "!";`. Phiền phức và dễ mắc lỗi, đặc biệt khi chuỗi trở nên phức tạp.

Theo lịch sử, các ngôn ngữ như Perl và PHP đã có tính năng nội suy. Java bắt kịp khá muộn. `String.format()` và `PrintStream.printf()` cung cấp chức năng tương tự, sử dụng các chỉ định định dạng giúp Java xử lý biến.

Các lựa chọn khác? Ngoài `String.format()`, chúng ta còn có `MessageFormat` và `StringBuilder`, nhưng chúng không mượt mà như nội suy cơ bản. Kể từ Java 15, khối văn bản đã đơn giản hóa chuỗi đa dòng và thêm `formatted()` để đơn giản hóa việc nội suy trực tiếp tại chỗ.

Về mặt triển khai, `String.format()` sử dụng `Formatter`, một công cụ mạnh mẽ với nhiều tùy chọn định dạng. Nhưng cảnh báo, chuỗi phức tạp có thể làm giảm hiệu suất ứng dụng của bạn nếu bạn không cẩn thận.

## Xem thêm
- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Formatter (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html)
- [JEP 378: Text Blocks (Final)](https://openjdk.java.net/jeps/378)
