---
aliases:
- /vi/java/searching-and-replacing-text/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:59.083509-07:00
description: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF text trong Java s\u1EBD vi\u1EBF\
  t \u0111\xE8 l\xEAn chu\u1ED7i g\u1ED1c b\u1EB1ng c\xE1c k\xFD t\u1EF1 m\u1EDBi\
  \ - h\xE3y ngh\u0129 v\u1EC1 n\xF3 nh\u01B0 l\xE0 vi\u1EC7c s\u1EED d\u1EE5ng l\u1EDB\
  p tr\u1EAFng \u0111i\u1EC7n t\u1EED. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: 2024-02-18 23:08:50.540473
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF text trong Java s\u1EBD vi\u1EBFt \u0111\
  \xE8 l\xEAn chu\u1ED7i g\u1ED1c b\u1EB1ng c\xE1c k\xFD t\u1EF1 m\u1EDBi - h\xE3\
  y ngh\u0129 v\u1EC1 n\xF3 nh\u01B0 l\xE0 vi\u1EC7c s\u1EED d\u1EE5ng l\u1EDBp tr\u1EAF\
  ng \u0111i\u1EC7n t\u1EED. C\xE1c l\u1EADp tr\xECnh\u2026"
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Tìm kiếm và thay thế text trong Java sẽ viết đè lên chuỗi gốc bằng các ký tự mới - hãy nghĩ về nó như là việc sử dụng lớp trắng điện tử. Các lập trình viên thường sử dụng tính năng này để làm sạch dữ liệu, điều chỉnh cài đặt, hoặc điều chỉnh lại thông điệp.

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
