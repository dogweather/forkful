---
title:                "写入标准错误"
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (是什么与为什么？)
写到标准错误 (stderr) 让你区分常规输出和错误消息。程序员这么做以确保错误处理不影响程序的正常输出。

## How to: (怎么做：)
```java
public class StdErrExample {
    public static void main(String[] args) {
        System.out.println("正常输出到stdout");
        System.err.println("错误信息到stderr");
    }
}
```
输出样本:
```
正常输出到stdout
错误信息到stderr
```
两行输出可能会按不同顺序出现，因为stdout和stderr是独立的。

## Deep Dive (深入探讨)
最初，UNIX提供了stdout和stderr来区别输出类型。写到stderr的好处是可以将错误重定向到日志或其他地方，而不打扰正常输出。替代方案如日志框架（如log4j）提供更复杂的错误管理。在Java中，`System.err`是`PrintStream`的一个实例，它默认链接到主机系统的标准错误输出渠道。

## See Also (另请参阅)
- [Java System Class Documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/System.html)
- [Oracle Java Tutorials – I/O Streams](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)
- [UNIX Standard Streams Wikipedia](https://en.wikipedia.org/wiki/Standard_streams)