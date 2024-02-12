---
title:                "提取子字符串"
aliases:
- /zh/java/extracting-substrings.md
date:                  2024-01-20T17:45:56.320820-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？以及为什么？)
提取子字符串意味着从一个较大的字符串中获取一小部分内容。程序员经常这么做来分析、变换数据或简化文本处理。

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
