---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:55:45.816899-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc viết hoa một chuỗi có nghĩa là biến chữ cái đầu tiên thành chữ hoa và các chữ cái còn lại thành chữ thường. Các lập trình viên sử dụng điều này để chuẩn hóa dữ liệu văn bản, như đầu vào của người dùng hay tên, đảm bảo tính nhất quán trong một tập dữ liệu.

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
