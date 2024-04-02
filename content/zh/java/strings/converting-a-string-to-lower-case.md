---
date: 2024-01-20 17:38:33.133888-07:00
description: "\u5728Java\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\
  \u610F\u5473\u7740\u5C06\u5176\u6240\u6709\u5B57\u7B26\u53D8\u4E3A\u5C0F\u5199\u5B57\
  \u6BCD\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\u4E86\u7EDF\u4E00\u6570\u636E\
  \u683C\u5F0F\uFF0C\u6BD4\u5982\u5728\u6BD4\u8F83\u548C\u641C\u7D22\u65F6\u5FFD\u7565\
  \u5927\u5C0F\u5199\u5DEE\u5F02\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.610709-06:00'
model: gpt-4-1106-preview
summary: "\u5728Java\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199\u610F\
  \u5473\u7740\u5C06\u5176\u6240\u6709\u5B57\u7B26\u53D8\u4E3A\u5C0F\u5199\u5B57\u6BCD\
  \u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u4E3A\u4E86\u7EDF\u4E00\u6570\u636E\u683C\
  \u5F0F\uFF0C\u6BD4\u5982\u5728\u6BD4\u8F83\u548C\u641C\u7D22\u65F6\u5FFD\u7565\u5927\
  \u5C0F\u5199\u5DEE\u5F02\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## What & Why? (是什么？为什么？)
在Java中将字符串转换为小写意味着将其所有字符变为小写字母。程序员这么做为了统一数据格式，比如在比较和搜索时忽略大小写差异。

## How to: (如何操作：)
Java使用`toLowerCase()`方法将字符串转为小写。下面是如何使用的例子：

```java
public class LowerCaseExample {
    public static void main(String[] args) {
        String myString = "Hello, WORLd!";
        String lowerCaseString = myString.toLowerCase();
        System.out.println(lowerCaseString);
    }
}
```

运行后输出：

```
hello, world!
```

## Deep Dive (深入探讨)
历史上，字符串小写转换是为了处理大小写不一致的问题，提高文本比较的效率和准确性。除了`toLowerCase()`，还有`Locale`敏感的方法`toLowerCase(Locale locale)`，它可以按照特定文化习俗来转换字母。实现这些方法时，Java使用Unicode标准来映射大写和小写版本的字符。

比如，在土耳其语中，大写的 I 不是 I 而是 İ，所以转换大小写时需要根据特定`Locale`来考虑。

```java
String str = "İstanbul";
System.out.println(str.toLowerCase(new Locale("tr", "TR"))); // istanbul
System.out.println(str.toLowerCase()); // i̇stanbul
```

你会看到不同的结果，因为默认`toLowerCase()`没有考虑土耳其的特殊情况。

## See Also (另请参阅)
- Java String类的官方文档：[https://docs.oracle.com/en/java/javase/](https://docs.oracle.com/en/java/javase/)
- Unicode字符表：[http://unicode.org/charts/](http://unicode.org/charts/)
- Locale类的官方文档：[https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
