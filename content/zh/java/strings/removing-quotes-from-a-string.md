---
date: 2024-01-26 03:40:00.800725-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8BA9\u6211\u4EEC\u5C06\u90A3\u4E9B\u8BA8\
  \u538C\u7684\u5F15\u53F7\u4ECE\u6587\u672C\u4E2D\u79FB\u9664\u3002\u6211\u4EEC\u5C06\
  \u4F7F\u7528 `replace()` \u65B9\u6CD5\u8FDB\u884C\u5FEB\u901F\u4FEE\u590D\uFF0C\u7528\
  \u6B63\u5219\u8868\u8FBE\u5F0F\u89E3\u51B3\u68D8\u624B\u95EE\u9898\u3002"
lastmod: '2024-04-05T21:53:47.933372-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4ECE\u5B57\u7B26\u4E32\u4E2D\u79FB\u9664\u5F15\u53F7"
weight: 9
---

## 如何操作：
让我们将那些讨厌的引号从文本中移除。我们将使用 `replace()` 方法进行快速修复，用正则表达式解决棘手问题。

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // 现在我们使用正则表达式，供模式爱好者使用
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## 深入了解
早些时候，字符串中的引号并不会引起太多问题——系统更简单，数据没有那么杂乱。随着复杂数据格式（JSON、XML）的出现及数据交换的需求，引号管理变得关键。说到替代方案，当然，你可以编写一个解析器，遍历每个字符，并构建一个新字符串（雨天可能会很有趣）。还有第三方库能够以更复杂的方式处理这一问题，提供逃逸字符而非移除它们的选项，或根据区域设置处理不同类型的引号。在实现上，要记住在没有上下文的情况下移除引号可能会改变数据的含义或结构——在考虑“如何做”之前，总是先考虑“为什么”。

## 另请参阅
- 想深入了解正则表达式，请查看官方 Java 文档：https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- 需要逃逸引号而不是移除它们？Stack Overflow 帮到你：https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- Java 中的 JSON 处理？你可能会经常遇到引号。这里是一个起点：https://www.oracle.com/technical-resources/articles/java/json.html
