---
date: 2024-01-20 17:47:33.555524-07:00
description: "How to: (\u600E\u4E48\u505A\uFF1F) \u5728Java\u5386\u53F2\u4E2D\uFF0C\
  `.length()`\u65B9\u6CD5\u662F\u51B3\u5B9A`String`\u5BF9\u8C61\u957F\u5EA6\u7684\u5E38\
  \u89C4\u65B9\u5F0F\uFF0C\u63D0\u4F9B\u4E86\u5B57\u7B26\u4E2A\u6570\u3002\u867D\u7136\
  \u5B57\u7B26\u4E32\u5B9E\u9645\u4E0A\u662F\u7531\u5B57\u7B26\u6570\u7EC4\u7EC4\u6210\
  \uFF0C\u5728\u5185\u90E8\uFF0C`.length`\u662F`char[]`\u7684\u5C5E\u6027\uFF0C\u4F46\
  Java\u5C01\u88C5\u4E86\u8FD9\u4E9B\u7EC6\u8282\u3002 \u66FF\u4EE3\u65B9\u6848? \u5176\
  \u4ED6\u8BED\u8A00\u4E2D\uFF0C\u5982Python,\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.936443-06:00'
model: gpt-4-1106-preview
summary: "(\u600E\u4E48\u505A\uFF1F) \u5728Java\u5386\u53F2\u4E2D\uFF0C`.length()`\u65B9\
  \u6CD5\u662F\u51B3\u5B9A`String`\u5BF9\u8C61\u957F\u5EA6\u7684\u5E38\u89C4\u65B9\
  \u5F0F\uFF0C\u63D0\u4F9B\u4E86\u5B57\u7B26\u4E2A\u6570\u3002\u867D\u7136\u5B57\u7B26\
  \u4E32\u5B9E\u9645\u4E0A\u662F\u7531\u5B57\u7B26\u6570\u7EC4\u7EC4\u6210\uFF0C\u5728\
  \u5185\u90E8\uFF0C`.length`\u662F`char[]`\u7684\u5C5E\u6027\uFF0C\u4F46Java\u5C01\
  \u88C5\u4E86\u8FD9\u4E9B\u7EC6\u8282."
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
