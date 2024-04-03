---
date: 2024-01-20 17:47:33.555524-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1F) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.615337-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

## How to: (怎么做？)
```Java
public class StringLengthExample {
    public static void main(String[] args) {
        String greeting = "你好，世界！";
        int length = greeting.length();
        System.out.println("字符串长度: " + length); // 输出字符串的长度
    }
}

/* 输出结果:
字符串长度: 6
*/
```

## Deep Dive (深入了解)
在Java历史中，`.length()`方法是决定`String`对象长度的常规方式，提供了字符个数。虽然字符串实际上是由字符数组组成，在内部，`.length`是`char[]`的属性，但Java封装了这些细节。

替代方案? 其他语言中，如Python, 可以使用`len()`函数。但在Java里，没有这样的替代方案——程序员通常使用`.length()`方法。

在Java 1.0版本中，字符串长度就用`.length()`找到。但不要和数组的`.length`属性混淆，它没有括号。

## See Also (另请参阅)
- [String (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Arrays (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html)
