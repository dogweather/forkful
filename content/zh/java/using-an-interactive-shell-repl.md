---
title:                "在编程中使用交互式Shell（REPL）"
date:                  2024-01-26T04:15:26.036549-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么与为什么？
REPL（读取-执行-打印循环）是一个交互式的命令行界面，可以处理单个用户输入、执行代码，并返回结果。程序员使用它进行快速实验、调试或学习，因为它允许立即反馈和迭代。

## 如何操作：
在Java中启动一个REPL非常简单，通过在Java 9中引入的`jshell`工具即可。以下是如何使用它并开始一个基本会话的方法：

```Java
jshell> int sum(int a, int b) {
   ...> return a + b;
   ...> }
|  创建了方法 sum(int,int)

jshell> sum(5, 7)
$1 ==> 12
```

随时通过`/exit`退出。

```Java
jshell> /exit
|  再见
```

## 深入了解
在`jshell`之前，Java程序员没有一个官方的REPL，不像Python或Ruby开发者那样。他们使用IDE或即便是为了微不足道的任务也必须编写完整的程序。随着Java 9的推出，`jshell`改变了游戏规则，弥补了这一差距。

其他选择包括在线编译器或IDE插件，但它们无法与`jshell`的即时性相比。至于内部机制，`jshell`使用Java编译器API来执行代码片段，这非常精巧。它不仅仅是一个游乐场——它可以导入库、定义类等等。这使得它成为一个健壮的原型设计工具。

## 另请参阅
- [JShell用户指南](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Java平台，标准版工具参考](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [Java编译器API](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
