---
title:                "字符串插值"
date:                  2024-01-20T17:50:59.920616-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么是字符串插值？为什么程序员要使用它？
字符串插值是把变量或表达式的值嵌入到字符串中的技术。程序员这么做，是为了创建动态和易于读写的信息。

## How to: 怎样进行字符串插值？
```java
public class StringInterpolationExample {
    public static void main(String[] args) {
        String name = "小明";
        int age = 25;
        
        // 使用String.format()
        String greeting = String.format("你好, %s。你今年 %d 岁。", name, age);
        System.out.println(greeting); // 输出: 你好, 小明。你今年 25 岁。
        
        // Java 15起, 可以使用文本块（Text Blocks）进行字符串插值
        String story = """
            %s正坐在房间里，此时他%s岁。
            """.formatted(name, age);
            
        System.out.println(story.trim()); // 输出: 小明正坐在房间里，此时他25岁。
    }
}
```

## Deep Dive 深入探讨
在Java历史上，字符串插值不像在其他一些语言中那么直接。初期，程序员通常会使用字符串连接操作符（`+`）来组合字符串和其他类型的值。这样的方法简单却低效，尤其是在拼接多个变量或处理大量数据时。

`String.format()`是后来提供的一个更强大的工具，它使用占位符和类型指定符，使得字符串模板和变量值的组合更加灵活和强大。Java 15 引入的文本块（Text Blocks）提供了一个新的方式来简化多行字符串的创建，并与`formatted()`方法结合使用，来实现更为简洁的字符串插值。

替代方案包括使用`StringBuilder`或`StringBuffer`进行字符串拼接，这对性能有所提升。

## See Also 相关资料链接
- [Oracle's Java Documentation on Strings](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [Java String.format() method](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html#format(java.lang.String,java.lang.Object...))
- [Oracle's introduction to Text Blocks](https://openjdk.java.net/jeps/378)
