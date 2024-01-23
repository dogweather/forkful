---
title:                "使用正则表达式"
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

使用正则表达式是为了匹配、搜索和替换文本模式。程序员用它因为它强大，可以处理复杂文本任务，提高效率。

## How to: (怎么用：)

### Example: 验证邮箱格式
```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String email = "example@example.com";
        String regex = "^[\\w.-]+@[\\w.-]+\\.[a-zA-Z]{2,6}$";
        
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(email);
        
        System.out.println(matcher.matches()); // 输出是否匹配
    }
}
```
输出：
```
true
```

### Example: 搜索数字
```java
public class RegexExample {
    public static void main(String[] args) {
        String document = "订单号12345, 金额: $678.90";
        String regex = "\\d+";  // 匹配一个或多个数字
        
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(document);
        
        while(matcher.find()) {
            System.out.println("找到数字: " + matcher.group());
        }
    }
}
```
输出：
```
找到数字: 12345
找到数字: 678
找到数字: 90
```

## Deep Dive (深入了解)

1. 历史：正则表达式由Stephen Kleene于1950年代发明，用于处理自动机理论。
2. 替代方案：使用字符串方法如 `.contains()`, `.split()`, `.indexOf()` 等，但这些不够灵活。
3. 实现细节：Java使用java.util.regex包，包括Pattern和Matcher类来实现正则表达式。

## See Also (另请参阅)

- Oracle 官方文档: [Pattern](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html)
- Oracle 官方文档: [Matcher](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Matcher.html)
- 正则表达式教程: [regular-expressions.info](https://www.regular-expressions.info/)
