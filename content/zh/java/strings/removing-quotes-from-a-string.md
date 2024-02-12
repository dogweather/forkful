---
title:                "从字符串中移除引号"
aliases:
- /zh/java/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:00.800725-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中移除引号"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
从字符串中移除引号意味着去除文本数据中的任何引号符号——单引号（' '）、双引号（" "）或两者都有。程序员这么做是为了清理输入，准备数据存储，或简化那些引号不必要且可能造成问题的解析任务。

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
