---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:43.989328-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Java\u7684\u6807\u51C6\u5E93\u6CA1\u6709\
  \u63D0\u4F9B\u76F4\u63A5\u4E00\u6B21\u6027\u5C06\u6574\u4E2A\u5B57\u7B26\u4E32\u5927\
  \u5199\u7684\u65B9\u6CD5\uFF0C\u4F46\u4F60\u53EF\u4EE5\u7ED3\u5408\u5185\u7F6E\u65B9\
  \u6CD5\u6765\u5B9E\u73B0\u8FD9\u4E00\u76EE\u6807\u3002\u5BF9\u4E8E\u66F4\u590D\u6742\
  \u7684\u9700\u6C42\uFF0C\u7B2C\u4E09\u65B9\u5E93\u50CFApache Commons Lang\u63D0\u4F9B\
  \u4E86\u76F4\u63A5\u7684\u89E3\u51B3\u65B9\u6848\u3002"
lastmod: '2024-04-05T21:53:47.928583-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

## 如何操作：
Java的标准库没有提供直接一次性将整个字符串大写的方法，但你可以结合内置方法来实现这一目标。对于更复杂的需求，第三方库像Apache Commons Lang提供了直接的解决方案。

### 使用Java的内置方法
不使用外部库来大写字符串，你可以将字符串分割成单词，将每个单词的首字母大写，然后再将它们重新连接。这里有一个简单的方法：

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // 输出: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

这段代码将整个字符串转换为小写，然后遍历每个字符，将每个单词的首字母大写。它将空格、句号和撇号视为单词分隔符。

### 使用Apache Commons Lang
Apache Commons Lang库使用`WordUtils.capitalizeFully()`方法提供了更优雅的解决方案，这个方法为你处理了各种边缘情况和分隔符：

```java
// 添加依赖: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // 输出: "Hello, World!"
    }
}
```

要使用这个方法，你需要将Apache Commons Lang库添加到你的项目中。这个库方法不仅将每个单词的首字母大写，还将每个单词中其余的字母转换为小写，确保整个字符串中的大小写模式一致。
