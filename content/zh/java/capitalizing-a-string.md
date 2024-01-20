---
title:                "字符串首字母大写"
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么?
大写字符串指的是将字符串中的所有小写字母转换成大写字母。程序员通常这样做来标准化文本输入，比如用户输入、数据处理前统一格式，或者当系统需要以一致的方式显示信息时。

## How to: 如何操作
```java
public class CapitalizeStringExample {
    public static void main(String[] args) {
        String original = "hello world!";
        String capitalized = original.toUpperCase();
        System.out.println(capitalized); // HELLO WORLD!
    }
}
```
输出:
```
HELLO WORLD!
```

## Deep Dive 深入探究
字符串大写在 Java 中相当简单。使用 `String` 类的 `toUpperCase()` 方法，就可以轻松实现。这个方法回溯到 Java 早期版本，提供了一种快速方便的方式来转换字符。


还有其他方法，比如通过 `StringBuilder` 或者遍历字符串中的每个字符来手动转换。但是这些方法的效率没有 `toUpperCase()` 高，因为 `toUpperCase()` 方法已经高度优化。还有 `Locale` 考虑： `toUpperCase(Locale locale)` 版本允许你针对特定语言环境（比如土耳其语）做出正确的字符转换。


字符串处理是编程中的常见任务。它涉及到的知识有字符编码、内部字符串表示（在 Java 中是 UTF-16）和性能考量。字符串操作，尤其是在大数据量时，可能会影响应用性能。因此，合理优化字符串使用是很重要的。

## See Also 相关链接
- Oracle官方文档: [String.toUpperCase()](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#toUpperCase())
- Java String API 指南: [Guide to java.lang.String](https://www.baeldung.com/java-string)
- 字符编码简介: [Understanding Character Encoding](https://www.w3schools.com/charsets/default.asp)