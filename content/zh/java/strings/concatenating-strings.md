---
title:                "字符串拼接"
aliases:
- /zh/java/concatenating-strings/
date:                  2024-01-20T17:35:07.995034-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
字符串连接就是将两个或多个字符串拼接成一个。程序员这么做是为了合成复杂的信息，或是将数据整理成易于阅读和处理的格式。

## How to: (如何操作：)
简单示例：
```Java
public class ConcatExample {
    public static void main(String[] args) {
        String firstName = "张";
        String lastName = "伟";
        String fullName = firstName + " " + lastName; // 使用 + 拼接字符串
        System.out.println("完整名字：" + fullName);
    }
}
```
输出：
```
完整名字：张 伟
```

使用StringBuilder：
```Java
public class StringBuilderExample {
    public static void main(String[] args) {
        StringBuilder builder = new StringBuilder();
        builder.append("张").append(" ").append("伟");
        String fullName = builder.toString();
        System.out.println("完整名字：" + fullName);
    }
}
```
输出：
```
完整名字：张 伟
```

## Deep Dive (深入探讨)
历史背景：字符串连接在Java诞生之初就已存在，早期版本使用+运算符或StringBuffer类。
替代方案：除了StringBuilder，我们还可以使用StringBuffer或String.join等方法。
实现细节：用+拼接字符串时，编译器实际使用StringBuilder来优化性能。但在循环内频繁连接字符串时使用StringBuilder直接可以提升效率。

## See Also (另请参阅)
- Oracle官方文档：[String](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html)
- Oracle官方文档：[StringBuilder](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/StringBuilder.html)
- Effective Java，第三版：探讨Java最佳实践
