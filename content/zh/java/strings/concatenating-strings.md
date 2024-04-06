---
date: 2024-01-20 17:35:07.995034-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u7B80\u5355\u793A\u4F8B\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.937431-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

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
