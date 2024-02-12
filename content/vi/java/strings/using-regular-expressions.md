---
title:                "Sử dụng biểu thức chính quy"
aliases:
- vi/java/using-regular-expressions.md
date:                  2024-01-28T22:10:03.553866-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là các mẫu được sử dụng để khớp các kết hợp ký tự trong văn bản. Lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa hoặc thao tác chuỗi một cách hiệu quả - tiết kiệm thời gian và dòng lệnh.

## Làm thế nào:
Để sử dụng regex trong Java, bạn cần các lớp `Pattern` và `Matcher` từ `java.util.regex`. Dưới đây là một ví dụ về việc tìm kiếm địa chỉ email trong một chuỗi.

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Contact me at hello@world.com or buzz@space.net.";
        String emailRegex = "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b";

        Pattern pattern = Pattern.compile(emailRegex);
        Matcher matcher = pattern.matcher(text);

        while (matcher.find()) {
            System.out.println(matcher.group());
        }
    }
}
```
Đầu ra:
```
hello@world.com
buzz@space.net
```

## Sâu hơn nữa
Biểu thức chính quy đã tồn tại từ những năm 1950, được phát minh bởi nhà toán học Stephen Kleene. Java đã tích hợp regex kể từ phiên bản 1.4. Mặc dù mạnh mẽ, nhưng regex có thể là quá mức cho những thao tác chuỗi đơn giản - các phương thức như `String.contains()`, `String.split()`, và `String.startsWith()` là lựa chọn đơn giản cho các kịch bản cơ bản. Bên dưới lớp vỏ, động cơ regex của Java (sử dụng `Pattern` và `Matcher`) biên dịch mẫu thành một loạt các chỉ thị byte code được thực thi bởi `Matcher` đối với chuỗi nhập vào.

## Xem thêm
Khám phá thêm về regex trong Java với những tài nguyên sau:
- [Lớp Pattern của Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- [Lớp Matcher của Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- [Hướng dẫn Java của Oracle: Biểu thức chính quy](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular-Expressions.info để tìm hiểu sâu về cú pháp và mẫu biểu thứ chính quy](https://www.regular-expressions.info/)
