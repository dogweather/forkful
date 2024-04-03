---
date: 2024-01-26 04:15:26.036549-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Java\u4E2D\u542F\u52A8\u4E00\u4E2A\
  REPL\u975E\u5E38\u7B80\u5355\uFF0C\u901A\u8FC7\u5728Java 9\u4E2D\u5F15\u5165\u7684\
  `jshell`\u5DE5\u5177\u5373\u53EF\u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\
  \u5B83\u5E76\u5F00\u59CB\u4E00\u4E2A\u57FA\u672C\u4F1A\u8BDD\u7684\u65B9\u6CD5\uFF1A\
  ."
lastmod: '2024-03-13T22:44:47.627176-06:00'
model: gpt-4-0125-preview
summary: "\u5728Java\u4E2D\u542F\u52A8\u4E00\u4E2AREPL\u975E\u5E38\u7B80\u5355\uFF0C\
  \u901A\u8FC7\u5728Java 9\u4E2D\u5F15\u5165\u7684`jshell`\u5DE5\u5177\u5373\u53EF\
  \u3002\u4EE5\u4E0B\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u5E76\u5F00\u59CB\u4E00\u4E2A\
  \u57FA\u672C\u4F1A\u8BDD\u7684\u65B9\u6CD5\uFF1A."
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

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
