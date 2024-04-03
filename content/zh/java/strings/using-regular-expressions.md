---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:14.404067-07:00
description: "Java\u4E2D\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u5141\
  \u8BB8\u4F60\u5B9A\u4E49\u7279\u5B9A\u6A21\u5F0F\u6765\u5728\u4EE3\u7801\u4E2D\u641C\
  \u7D22\u3001\u64CD\u4F5C\u6216\u9A8C\u8BC1\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\
  \u4F7F\u7528\u5B83\u4EEC\u6765\u6267\u884C\u50CF\u89E3\u6790\u65E5\u5FD7\u6587\u4EF6\
  \u3001\u9A8C\u8BC1\u7528\u6237\u8F93\u5165\u6216\u5728\u6587\u672C\u4E2D\u641C\u7D22\
  \u7279\u5B9A\u6A21\u5F0F\u7684\u4EFB\u52A1\uFF0C\u4ECE\u800C\u4EE5\u6700\u5C0F\u7684\
  \u52AA\u529B\u5B9E\u73B0\u590D\u6742\u7684\u5B57\u7B26\u4E32\u5904\u7406\u3002"
lastmod: '2024-03-13T22:44:47.614242-06:00'
model: gpt-4-0125-preview
summary: "Java\u4E2D\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u5141\u8BB8\
  \u4F60\u5B9A\u4E49\u7279\u5B9A\u6A21\u5F0F\u6765\u5728\u4EE3\u7801\u4E2D\u641C\u7D22\
  \u3001\u64CD\u4F5C\u6216\u9A8C\u8BC1\u5B57\u7B26\u4E32\u3002\u7A0B\u5E8F\u5458\u4F7F\
  \u7528\u5B83\u4EEC\u6765\u6267\u884C\u50CF\u89E3\u6790\u65E5\u5FD7\u6587\u4EF6\u3001\
  \u9A8C\u8BC1\u7528\u6237\u8F93\u5165\u6216\u5728\u6587\u672C\u4E2D\u641C\u7D22\u7279\
  \u5B9A\u6A21\u5F0F\u7684\u4EFB\u52A1\uFF0C\u4ECE\u800C\u4EE5\u6700\u5C0F\u7684\u52AA\
  \u529B\u5B9E\u73B0\u590D\u6742\u7684\u5B57\u7B26\u4E32\u5904\u7406\u3002."
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
weight: 11
---

## 如何操作：
Java对regex的内置支持主要通过`java.util.regex`包中的`Pattern`和`Matcher`类。这里有一个简单的示例，用于查找并打印字符串中所有出现的单词，不区分大小写：

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("在位置 " + matcher.start() + " 找到 '" + matcher.group() + "'");
        }
    }
}
```

输出：
```
在位置 16 找到 'parsing'
在位置 31 找到 'Parsing'
```

对于像字符串分割这样的任务，你可以使用`String`类的`split()`方法与regex一起使用：

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

输出：
```
Java
Python
Ruby
JavaScript
```

在Java中使用regex时，可能会有一些情况下使用外部库可以简化复杂任务。一个用于Java中处理regex的流行第三方库是`Apache Commons Lang`。它提供了像`StringUtils`这样的实用程序，使某些regex任务更加简单。以下是如何使用它来计算子字符串的匹配次数：

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "' 出现了 " + count + " 次。");
    }
}
```

要使用Apache Commons Lang，你需要将其包含在你的项目中。如果你使用Maven，将这个依赖项添加到你的`pom.xml`中：

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- 检查最新版本 -->
</dependency>
```

输出：
```
'processing' 出现了 2 次。
```
