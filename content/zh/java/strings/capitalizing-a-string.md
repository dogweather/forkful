---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:43.989328-07:00
description: "\u5B57\u7B26\u4E32\u7684\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u4FEE\
  \u6539\u5B57\u7B26\u4E32\u4E2D\u6BCF\u4E2A\u5355\u8BCD\u7684\u9996\u5B57\u6BCD\u4E3A\
  \u5927\u5199\uFF0C\u540C\u65F6\u786E\u4FDD\u5176\u4F59\u7684\u5B57\u6BCD\u4FDD\u6301\
  \u5C0F\u5199\u3002\u8FD9\u79CD\u5E38\u89C1\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u4EFB\
  \u52A1\u5BF9\u4E8E\u683C\u5F0F\u5316\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u6587\u672C\
  \u975E\u5E38\u6709\u7528\uFF0C\u6BD4\u5982\u6309\u7167\u7EA6\u5B9A\u6216\u8BED\u6CD5\
  \u6B63\u786E\u6027\u51C6\u5907\u7528\u6237\u540D\u79F0\u6216\u6807\u9898\u3002"
lastmod: '2024-02-25T18:49:45.170119-07:00'
model: gpt-4-0125-preview
summary: "\u5B57\u7B26\u4E32\u7684\u9996\u5B57\u6BCD\u5927\u5199\u6D89\u53CA\u4FEE\
  \u6539\u5B57\u7B26\u4E32\u4E2D\u6BCF\u4E2A\u5355\u8BCD\u7684\u9996\u5B57\u6BCD\u4E3A\
  \u5927\u5199\uFF0C\u540C\u65F6\u786E\u4FDD\u5176\u4F59\u7684\u5B57\u6BCD\u4FDD\u6301\
  \u5C0F\u5199\u3002\u8FD9\u79CD\u5E38\u89C1\u7684\u5B57\u7B26\u4E32\u64CD\u4F5C\u4EFB\
  \u52A1\u5BF9\u4E8E\u683C\u5F0F\u5316\u5E94\u7528\u7A0B\u5E8F\u4E2D\u7684\u6587\u672C\
  \u975E\u5E38\u6709\u7528\uFF0C\u6BD4\u5982\u6309\u7167\u7EA6\u5B9A\u6216\u8BED\u6CD5\
  \u6B63\u786E\u6027\u51C6\u5907\u7528\u6237\u540D\u79F0\u6216\u6807\u9898\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串的首字母大写涉及修改字符串中每个单词的首字母为大写，同时确保其余的字母保持小写。这种常见的字符串操作任务对于格式化应用程序中的文本非常有用，比如按照约定或语法正确性准备用户名称或标题。

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
