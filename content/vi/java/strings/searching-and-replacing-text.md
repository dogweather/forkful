---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:59.083509-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: T\xECm ki\u1EBFm v\xE0 thay th\u1EBF trong\
  \ Java r\u1EA5t d\u1EC5 d\xE0ng nh\u1EDD v\xE0o l\u1EDBp `String` v\xE0 ph\u01B0\
  \u01A1ng th\u1EE9c `replace()` c\u1EE7a n\xF3. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ c\xE1ch b\u1EA1n l\xE0m."
lastmod: '2024-03-13T22:44:36.469568-06:00'
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF trong Java r\u1EA5t d\u1EC5 d\xE0ng\
  \ nh\u1EDD v\xE0o l\u1EDBp `String` v\xE0 ph\u01B0\u01A1ng th\u1EE9c `replace()`\
  \ c\u1EE7a n\xF3."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Cách thực hiện:
Tìm kiếm và thay thế trong Java rất dễ dàng nhờ vào lớp `String` và phương thức `replace()` của nó. Dưới đây là cách bạn làm:

```java
public class ReplaceDemo {
    public static void main(String[] args) {
        String originalText = "The quick brown fox jumps over the lazy dog";
        String modifiedText = originalText.replace("lazy", "energetic");
        
        System.out.println("Trước: " + originalText);
        System.out.println("Sau: " + modifiedText);
    }
}
```

Kết quả:
```
Trước: The quick brown fox jumps over the lazy dog
Sau: The quick brown fox jumps over the energetic dog
```

Bây giờ, đối với các mẫu hoặc việc thay thế phức tạp hơn, `Pattern` và `Matcher` được sử dụng:

```java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexReplaceDemo {
    public static void main(String[] args) {
        String originalText = "There are 31,536,000 seconds in 365 days.";
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(originalText);
        String modifiedText = matcher.replaceAll("#");
        
        System.out.println("Trước: " + originalText);
        System.out.println("Sau: " + modifiedText);        
    }
}
```

Kết quả:
```
Trước: There are 31,536,000 seconds in 365 days.
Sau: There are # seconds in # days.
```

## Sâu hơn:
Phương thức `replace()` có nguồn gốc từ những ngày đầu của Java. Nó là một phần của lớp `String` bất biến, nghĩa là mỗi lần bạn sử dụng nó, bạn đang tạo một chuỗi mới. Rất thân thiện với môi trường, không lãng phí đồ cũ.

Nhưng vấn đề với `Pattern` và `Matcher` là gì, bạn hỏi? Các lớp này là một phần của API biểu thức chính quy (regex) của Java, được giới thiệu trong Java 1.4. Chúng tăng cường khả năng tìm kiếm và thay thế, cho phép bạn phát hiện các mẫu phức tạp và chỉnh sửa văn bản một cách linh hoạt. Đó giống như việc sử dụng một dao mổ thay vì một búa lớn.

Thêm vào đó, có `replaceAll()` và `replaceFirst()`, hai phương thức của lớp `Matcher` giúp bạn điều chỉnh việc biến đổi văn bản một cách tinh tế hơn, thay thế tất cả các lần xuất hiện hoặc chỉ lần xuất hiện đầu tiên.

Một lựa chọn khác là sử dụng lớp `StringBuffer` hoặc `StringBuilder` khi bạn đang xử lý hàng tấn sửa đổi vì khác với `String`, các bộ đệm này có thể thay đổi.

## Xem thêm:
- [Tài liệu Java String](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Tài liệu Java Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Tài liệu Matcher](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Hướng dẫn về Biểu thức Chính quy](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)

Để thực hành nhiều hơn, hãy kiểm tra RegexOne (https://regexone.com), đó là một nguồn lực tuyệt vời để nâng cấp kỹ năng regex của bạn.
