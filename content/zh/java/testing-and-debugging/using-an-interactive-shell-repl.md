---
date: 2024-01-26 04:15:26.036549-07:00
description: "REPL\uFF08\u8BFB\u53D6-\u6267\u884C-\u6253\u5370\u5FAA\u73AF\uFF09\u662F\
  \u4E00\u4E2A\u4EA4\u4E92\u5F0F\u7684\u547D\u4EE4\u884C\u754C\u9762\uFF0C\u53EF\u4EE5\
  \u5904\u7406\u5355\u4E2A\u7528\u6237\u8F93\u5165\u3001\u6267\u884C\u4EE3\u7801\uFF0C\
  \u5E76\u8FD4\u56DE\u7ED3\u679C\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\
  \u5FEB\u901F\u5B9E\u9A8C\u3001\u8C03\u8BD5\u6216\u5B66\u4E60\uFF0C\u56E0\u4E3A\u5B83\
  \u5141\u8BB8\u7ACB\u5373\u53CD\u9988\u548C\u8FED\u4EE3\u3002"
lastmod: '2024-03-13T22:44:47.627176-06:00'
model: gpt-4-0125-preview
summary: "REPL\uFF08\u8BFB\u53D6-\u6267\u884C-\u6253\u5370\u5FAA\u73AF\uFF09\u662F\
  \u4E00\u4E2A\u4EA4\u4E92\u5F0F\u7684\u547D\u4EE4\u884C\u754C\u9762\uFF0C\u53EF\u4EE5\
  \u5904\u7406\u5355\u4E2A\u7528\u6237\u8F93\u5165\u3001\u6267\u884C\u4EE3\u7801\uFF0C\
  \u5E76\u8FD4\u56DE\u7ED3\u679C\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u5B83\u8FDB\u884C\
  \u5FEB\u901F\u5B9E\u9A8C\u3001\u8C03\u8BD5\u6216\u5B66\u4E60\uFF0C\u56E0\u4E3A\u5B83\
  \u5141\u8BB8\u7ACB\u5373\u53CD\u9988\u548C\u8FED\u4EE3\u3002."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
