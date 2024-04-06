---
date: 2024-01-20 17:35:07.995034-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u80CC\u666F\uFF1A\
  \u5B57\u7B26\u4E32\u8FDE\u63A5\u5728Java\u8BDE\u751F\u4E4B\u521D\u5C31\u5DF2\u5B58\
  \u5728\uFF0C\u65E9\u671F\u7248\u672C\u4F7F\u7528+\u8FD0\u7B97\u7B26\u6216StringBuffer\u7C7B\
  \u3002 \u66FF\u4EE3\u65B9\u6848\uFF1A\u9664\u4E86StringBuilder\uFF0C\u6211\u4EEC\
  \u8FD8\u53EF\u4EE5\u4F7F\u7528StringBuffer\u6216String.join\u7B49\u65B9\u6CD5\u3002\
  \u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.823792-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u80CC\u666F\uFF1A\u5B57\u7B26\
  \u4E32\u8FDE\u63A5\u5728Java\u8BDE\u751F\u4E4B\u521D\u5C31\u5DF2\u5B58\u5728\uFF0C\
  \u65E9\u671F\u7248\u672C\u4F7F\u7528+\u8FD0\u7B97\u7B26\u6216StringBuffer\u7C7B\u3002"
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
