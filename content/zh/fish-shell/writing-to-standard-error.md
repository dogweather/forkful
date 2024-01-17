---
title:                "“写入标准错误”"
html_title:           "Fish Shell: “写入标准错误”"
simple_title:         "“写入标准错误”"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

写入标准错误是指在编程过程中，将错误信息或警告信息输出到终端的标准错误流中。程序员进行这样的操作是为了及时发现和解决问题，避免代码出错而导致程序崩溃。

## 如何：
下面是通过Fish Shell将错误信息输出到标准错误流的示例代码和输出结果：
```
Fish Shell ...
- 命令：ls bad_file
- 错误信息输出：ls: cannot access 'bad_file': No such file or directory
```
该代码中，通过使用Fish Shell的错误输出函数，我们可以直接将错误信息输出到终端，从而快速发现问题所在。

## 深入了解：
在早期的编程语言中，并没有将错误信息输出到标准错误流的概念，所有的信息都被输出到标准输出流中。但是随着程序复杂性的提高，及时发现和解决问题变得越来越重要，因此就出现了将错误信息输出到标准错误流的操作。除了使用Fish Shell的错误输出函数外，也可以利用一些其他的技巧来实现将错误信息输出到标准错误流，如重定向操作符“2>”。

## 参考链接：
- Fish Shell文档：https://fishshell.com/docs/current/tutorial.html#tut_errors
- Standard Error和Standard Output的区别：https://www.marksanborn.net/unix-tutorial-for-beginners/basic-unix-commands/the-difference-between-stdout-and-stderr/