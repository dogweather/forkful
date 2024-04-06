---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:14.404067-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java\u5BF9regex\u7684\u5185\u7F6E\u652F\
  \u6301\u4E3B\u8981\u901A\u8FC7`java.util.regex`\u5305\u4E2D\u7684`Pattern`\u548C\
  `Matcher`\u7C7B\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u7B80\u5355\u7684\u793A\u4F8B\
  \uFF0C\u7528\u4E8E\u67E5\u627E\u5E76\u6253\u5370\u5B57\u7B26\u4E32\u4E2D\u6240\u6709\
  \u51FA\u73B0\u7684\u5355\u8BCD\uFF0C\u4E0D\u533A\u5206\u5927\u5C0F\u5199\uFF1A."
lastmod: '2024-04-05T21:53:47.935518-06:00'
model: gpt-4-0125-preview
summary: ''
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
