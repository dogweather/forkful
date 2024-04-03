---
date: 2024-01-20 17:38:33.133888-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) Java\u4F7F\u7528`toLowerCase()`\u65B9\
  \u6CD5\u5C06\u5B57\u7B26\u4E32\u8F6C\u4E3A\u5C0F\u5199\u3002\u4E0B\u9762\u662F\u5982\
  \u4F55\u4F7F\u7528\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.610709-06:00'
model: gpt-4-1106-preview
summary: "Java\u4F7F\u7528`toLowerCase()`\u65B9\u6CD5\u5C06\u5B57\u7B26\u4E32\u8F6C\
  \u4E3A\u5C0F\u5199\u3002\u4E0B\u9762\u662F\u5982\u4F55\u4F7F\u7528\u7684\u4F8B\u5B50\
  \uFF1A."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

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
