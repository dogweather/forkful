---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:29.822816-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n tr\u1EF1c ti\u1EBF\
  p c\xE1c bi\u1EBFn v\xE0o trong chu\u1ED7i. \u0110i\u1EC1u n\xE0y l\xE0m cho m\xE3\
  \ l\u1EC7nh tr\u1EDF n\xEAn g\u1ECDn g\xE0ng v\xE0 d\u1EC5 \u0111\u1ECDc h\u01A1\
  n b\u1EB1ng c\xE1ch tr\xE1nh vi\u1EC7c n\u1ED1i chu\u1ED7i\u2026"
lastmod: '2024-03-11T00:14:09.735070-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n tr\u1EF1c ti\u1EBF\
  p c\xE1c bi\u1EBFn v\xE0o trong chu\u1ED7i. \u0110i\u1EC1u n\xE0y l\xE0m cho m\xE3\
  \ l\u1EC7nh tr\u1EDF n\xEAn g\u1ECDn g\xE0ng v\xE0 d\u1EC5 \u0111\u1ECDc h\u01A1\
  n b\u1EB1ng c\xE1ch tr\xE1nh vi\u1EC7c n\u1ED1i chu\u1ED7i\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
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
