---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么& 为什么？ 
删除匹配模式的字符是指在字符串中查找并清除符合特定规则（模式）的字符。程序员通常这样做以筛选或清理输入，从而有效管理数据。

## 如何做:
让我们通过一个简单的示例来演示如何在Java中删除匹配的字符。使用`String.replaceAll()`方法和正则表达式可以很容易地完成这个任务。

```Java
public class Main {
    public static void main(String[] args) {
        String text = "Hello,2 World!3";
        String pattern = "\\d";  // 删掉所有数字
        String cleanedText = text.replaceAll(pattern, "");
        System.out.println(cleanedText);
    }
}
```
输出会是：

```Java
Hello, World!
```

## 深入了解 
1) 历史背景：早在UNIX时代，就利用sed和awk等工具进行模式匹配和字符删除。Java继续这种范例，并提供了更快、更强大的工具。

2) 替代方案：除了`String.replaceAll()`方法，还可以使用`StringBuffer`或`StringBuilder`配合`deleteCharAt()`方法来删除匹配的字符。或者如果要增加效率，可以使用第三方库，如Apache Commons Lang的`StringUtils`类。

3) 实现细节：`replaceAll()`方法实际上将原始字符串分割成多个部分，然后在不匹配模式的地方重新连接这些部分。这就是为何它会比逐个删除字符稍慢一些。

## 另请参阅 
- Java官方文档的`String.replaceAll()`方法：https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html#replaceAll(java.lang.String,java.lang.String)
- Apache Commons Lang的`StringUtils`类：https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html
- 正则表达式入门教程：https://www.runoob.com/regexp/regexp-tutorial.html