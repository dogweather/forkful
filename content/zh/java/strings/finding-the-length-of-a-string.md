---
date: 2024-01-20 17:47:33.555524-07:00
description: "\u5728Java\u4E2D\u8BA1\u7B97\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\uFF0C\
  \u5C31\u662F\u77E5\u9053\u5B57\u7B26\u4E32\u5305\u542B\u591A\u5C11\u4E2A\u5B57\u7B26\
  \u3002\u7A0B\u5E8F\u5458\u9700\u8981\u8FD9\u4E2A\u4FE1\u606F\u6765\u8FDB\u884C\u5FAA\
  \u73AF\u3001\u9A8C\u8BC1\u6570\u636E\uFF0C\u6216\u8005\u5904\u7406\u683C\u5F0F\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.179059-07:00'
model: gpt-4-1106-preview
summary: "\u5728Java\u4E2D\u8BA1\u7B97\u5B57\u7B26\u4E32\u7684\u957F\u5EA6\uFF0C\u5C31\
  \u662F\u77E5\u9053\u5B57\u7B26\u4E32\u5305\u542B\u591A\u5C11\u4E2A\u5B57\u7B26\u3002\
  \u7A0B\u5E8F\u5458\u9700\u8981\u8FD9\u4E2A\u4FE1\u606F\u6765\u8FDB\u884C\u5FAA\u73AF\
  \u3001\u9A8C\u8BC1\u6570\u636E\uFF0C\u6216\u8005\u5904\u7406\u683C\u5F0F\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
在Java中计算字符串的长度，就是知道字符串包含多少个字符。程序员需要这个信息来进行循环、验证数据，或者处理格式。

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
