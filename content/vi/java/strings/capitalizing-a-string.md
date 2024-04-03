---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:45.816899-07:00
description: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129a l\xE0 bi\u1EBF\
  n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn th\xE0nh ch\u1EEF hoa v\xE0 c\xE1c ch\u1EEF\
  \ c\xE1i c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDDng. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng \u0111i\u1EC1u n\xE0y \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.466847-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt hoa m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129a l\xE0 bi\u1EBF\
  n ch\u1EEF c\xE1i \u0111\u1EA7u ti\xEAn th\xE0nh ch\u1EEF hoa v\xE0 c\xE1c ch\u1EEF\
  \ c\xE1i c\xF2n l\u1EA1i th\xE0nh ch\u1EEF th\u01B0\u1EDDng."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
Trong Java, không có phương thức sẵn có để viết hoa một chuỗi hoàn toàn (chữ cái đầu tiên viết hoa, phần còn lại viết thường), nhưng đây là một hàm nhanh để làm chính điều đó:

```java
public class StringCapitalizer {
    public static void main(String[] args) {
        String input = "java is fun!"; // chuỗi ví dụ
        String output = capitalizeString(input);
        System.out.println(output); // Java is fun!
    }

    public static String capitalizeString(String str) {
        if(str == null || str.isEmpty()) {
            return str;
        }
        return str.substring(0, 1).toUpperCase() + str.substring(1).toLowerCase();
    }
}
```

## Sâu hơn nữa
Trước Java 8, phương pháp ở trên là một cách phổ biến để viết hoa một chuỗi. Kể từ khi giới thiệu streams trong Java 8, chúng ta cũng có thể thao tác chuỗi với sự linh hoạt hơn.

Một cách thay thế để viết hoa sử dụng streams:

```java
import java.util.stream.*;

public class StringCapitalizer {
    public static void main(String[] args) {
        String input = "java is cool!";
        String output = Arrays.stream(input.split("\\s"))
                              .map(word -> word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase())
                              .collect(Collectors.joining(" "));
        System.out.println(output); // Java Is Cool!
    }
}
```

Cách này tách chuỗi thành các từ, viết hoa từng từ và nối chúng lại với nhau. Lưu ý sự khác biệt: mỗi từ được viết hoa, không chỉ chữ cái đầu tiên.

Chuỗi trong Java là bất biến - có nghĩa là, một khi nó được tạo ra, chúng không thể thay đổi. Các phương pháp có vẻ như thay đổi chuỗi, như `toUpperCase` hoặc `toLowerCase`, thực sự tạo ra các chuỗi mới với các thay đổi được áp dụng.

Về mặt hiệu suất, StringBuilder thường được sử dụng cho việc thao tác chuỗi, bởi vì nó có thể thay đổi. Nó tránh được chi phí tạo ra nhiều đối tượng chuỗi. Tuy nhiên, đối với việc viết hoa đơn giản, lợi ích về hiệu suất không lớn, do đó, một ví dụ về `StringBuilder` được bỏ qua.

## Xem thêm
- [Tài liệu API Chuỗi Java](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Tài liệu Collector](https://docs.oracle.com/javase/8/docs/api/java/util/stream/Collectors.html)
- [Tài liệu StringJoiner](https://docs.oracle.com/javase/8/docs/api/java/util/StringJoiner.html)
