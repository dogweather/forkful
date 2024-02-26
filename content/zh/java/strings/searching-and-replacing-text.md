---
date: 2024-01-20 17:58:20.415457-07:00
description: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5141\u8BB8\u6211\u4EEC\u5B9A\
  \u4F4D\u7279\u5B9A\u5B57\u7B26\u4E32\uFF0C\u7136\u540E\u7528\u5176\u4ED6\u5B57\u7B26\
  \u4E32\u6765\u66F4\u65B0\u5B83\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u4FEE\u6539\u4EE3\u7801\u3001\u6570\u636E\u6E05\u6D17\uFF0C\u6216\u6279\u91CF\
  \u66F4\u65B0\u6587\u6863\u5185\u5BB9\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.172676-07:00'
model: gpt-4-1106-preview
summary: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C\u5141\u8BB8\u6211\u4EEC\u5B9A\
  \u4F4D\u7279\u5B9A\u5B57\u7B26\u4E32\uFF0C\u7136\u540E\u7528\u5176\u4ED6\u5B57\u7B26\
  \u4E32\u6765\u66F4\u65B0\u5B83\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u4FEE\u6539\u4EE3\u7801\u3001\u6570\u636E\u6E05\u6D17\uFF0C\u6216\u6279\u91CF\
  \u66F4\u65B0\u6587\u6863\u5185\u5BB9\u3002"
title: "\u641C\u7D22\u548C\u66FF\u6362\u6587\u672C"
---

{{< edit_this_page >}}

## 什么 & 为什么？
搜索和替换文本允许我们定位特定字符串，然后用其他字符串来更新它。程序员这么做是为了修改代码、数据清洗，或批量更新文档内容。

## 怎么做：
```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SearchAndReplace {
    public static void main(String[] args) {
        String text = "Hello, World! Hello, Java World!";
        String searchFor = "World";
        String replaceWith = "Programmer";

        String replacedText = text.replace(searchFor, replaceWith);
        System.out.println(replacedText);  // Outputs: Hello, Programmer! Hello, Java Programmer!

        // 用正则表达式来搜索和替换
        String regexText = "Coding in 2023, Coding in Java!";
        Pattern pattern = Pattern.compile("\\d{4}");
        Matcher matcher = pattern.matcher(regexText);
        String updatedText = matcher.replaceAll("202X");
        System.out.println(updatedText);  // Outputs: Coding in 202X, Coding in Java!
    }
}
```

## 深入探究
文本搜索和替换最早出现在文本编辑器中，如vi和emacs，而现代编程语言则内置了这些功能。除了上面例子中用到的`String.replace`和正则表达式外，有些库提供了更强大的文本处理能力，例如Apache Commons Lang和Google Guava。实现细节方面，性能很大程度上取决于搜索算法（如KMP算法）和正则表达式引擎。

## 另见
- Oracle官方教程关于正则表达式：[https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Apache Commons Lang：[https://commons.apache.org/proper/commons-lang/](https://commons.apache.org/proper/commons-lang/)
- Google Guava库：[https://github.com/google/guava](https://github.com/google/guava)
