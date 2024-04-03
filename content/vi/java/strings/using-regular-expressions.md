---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:03.553866-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 s\u1EED d\u1EE5ng regex trong Java,\
  \ b\u1EA1n c\u1EA7n c\xE1c l\u1EDBp `Pattern` v\xE0 `Matcher` t\u1EEB `java.util.regex`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 v\u1EC1 vi\u1EC7c t\xEC\
  m ki\u1EBFm \u0111\u1ECBa ch\u1EC9\u2026"
lastmod: '2024-03-13T22:44:36.476017-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 s\u1EED d\u1EE5ng regex trong Java, b\u1EA1n c\u1EA7n c\xE1\
  c l\u1EDBp `Pattern` v\xE0 `Matcher` t\u1EEB `java.util.regex`."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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
