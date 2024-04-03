---
date: 2024-01-20 17:45:56.320820-07:00
description: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u610F\u5473\u7740\u4ECE\u4E00\u4E2A\
  \u8F83\u5927\u7684\u5B57\u7B26\u4E32\u4E2D\u83B7\u53D6\u4E00\u5C0F\u90E8\u5206\u5185\
  \u5BB9\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FD9\u4E48\u505A\u6765\u5206\u6790\u3001\
  \u53D8\u6362\u6570\u636E\u6216\u7B80\u5316\u6587\u672C\u5904\u7406\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.613041-06:00'
model: gpt-4-1106-preview
summary: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u610F\u5473\u7740\u4ECE\u4E00\u4E2A\
  \u8F83\u5927\u7684\u5B57\u7B26\u4E32\u4E2D\u83B7\u53D6\u4E00\u5C0F\u90E8\u5206\u5185\
  \u5BB9\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FD9\u4E48\u505A\u6765\u5206\u6790\u3001\
  \u53D8\u6362\u6570\u636E\u6216\u7B80\u5316\u6587\u672C\u5904\u7406\u3002."
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

## How to: (如何操作：)
在Java中提取子字符串，我们使用`substring()`方法。来看几个例子。

```Java
public class SubstringExample {
    public static void main(String[] args) {
        String originalString = "Hello, 世界!";
        
        // 提取从索引1开始到5结束的子串
        String extracted = originalString.substring(1, 5);
        System.out.println(extracted); // 输出: ello
        
        // 从索引7开始到字符串末尾的子串
        String endExtract = originalString.substring(7);
        System.out.println(endExtract); // 输出: 世界!
    }
}
```

## Deep Dive (深入探索)
提取子字符串的功能在Java初版时就存在了。这是个非常基础但又不可或缺的工具。`substring()`方法在Java 1中就出现了，并在后续的版本中持续改进。有其他方法可以实现相似的功能，如`StringUtils`类中的`mid()`, `left()`, `right()`等方法（Apache Commons Lang库中）。此外，Java的`Pattern`和`Matcher`类提供了正则表达式的强大功能来进行复杂的文本提取。

## See Also (另请参阅)
- [Java String documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html)
- [Oracle Java tutorials – String](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
