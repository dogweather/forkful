---
aliases:
- /zh/java/reading-command-line-arguments/
date: 2024-01-20 17:56:08.389480-07:00
description: "\u5728Java\u7A0B\u5E8F\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\
  \u5C31\u662F\u83B7\u53D6\u7528\u6237\u5728\u542F\u52A8\u7A0B\u5E8F\u65F6\u4F20\u5165\
  \u7684\u4FE1\u606F\u3002\u6211\u4EEC\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8BA9\u7A0B\
  \u5E8F\u66F4\u7075\u6D3B\uFF0C\u80FD\u6839\u636E\u4E0D\u540C\u8F93\u5165\u6267\u884C\
  \u4E0D\u540C\u4EFB\u52A1\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.034639
model: gpt-4-1106-preview
summary: "\u5728Java\u7A0B\u5E8F\u4E2D\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570\u5C31\
  \u662F\u83B7\u53D6\u7528\u6237\u5728\u542F\u52A8\u7A0B\u5E8F\u65F6\u4F20\u5165\u7684\
  \u4FE1\u606F\u3002\u6211\u4EEC\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u8BA9\u7A0B\u5E8F\
  \u66F4\u7075\u6D3B\uFF0C\u80FD\u6839\u636E\u4E0D\u540C\u8F93\u5165\u6267\u884C\u4E0D\
  \u540C\u4EFB\u52A1\u3002"
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在Java程序中读取命令行参数就是获取用户在启动程序时传入的信息。我们这么做是为了让程序更灵活，能根据不同输入执行不同任务。

## How to: (如何操作：)
```java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("命令行参数如下：");
            for(String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("未提供命令行参数。");
        }
    }
}

```
如果运行 `java CommandLineExample Hello World`，输出将是：
```
命令行参数如下：
Hello
World
```

## Deep Dive (深入探究)
命令行参数的使用可追溯到早期计算机时代，那时几乎所有交互都通过命令行进行。虽然图形用户界面（GUIs）变得普遍，但命令行参数在自动化和脚本编写中仍扮演着关键角色。

替代方案包括读取环境变量、配置文件、或者通过图形用户界面的输入。命令行参数通过 `main` 方法的 `args` 数组传入，这是一个字符串数组，其元素顺序与用户输入的命令行文本顺序相同。

实现细节方面，`args` 数组并不局限于简单的字符串。可以解析并将它们转换为其他数据类型来满足程序需求。但要小心处理，因为错误的输入可能会导致运行时异常。

## See Also (另请参阅)
- [Oracle官方文档 - 命令行参数](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/) - 一个用于解析命令行参数的库
- [JCommander](http://jcommander.org/) - 更现代的命令行参数解析库

请记住，了解你的工具可以让你编写出更加强大、灵活的程序。别小看了命令行参数，它们简单但功能强大！
