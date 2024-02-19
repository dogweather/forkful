---
aliases:
- /zh/java/concatenating-strings/
date: 2024-01-20 17:35:07.995034-07:00
description: "\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\u5C06\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u5408\u6210\u590D\u6742\u7684\u4FE1\u606F\uFF0C\u6216\
  \u662F\u5C06\u6570\u636E\u6574\u7406\u6210\u6613\u4E8E\u9605\u8BFB\u548C\u5904\u7406\
  \u7684\u683C\u5F0F\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.013760
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u8FDE\u63A5\u5C31\u662F\u5C06\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u62FC\u63A5\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u4E3A\u4E86\u5408\u6210\u590D\u6742\u7684\u4FE1\u606F\uFF0C\u6216\
  \u662F\u5C06\u6570\u636E\u6574\u7406\u6210\u6613\u4E8E\u9605\u8BFB\u548C\u5904\u7406\
  \u7684\u683C\u5F0F\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
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
