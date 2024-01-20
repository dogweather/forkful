---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么?

启动新项目是创建新的计算机程序的过程，并从零开始。程序员之所以这样做，主要是为了解决问题，或者创建一些新的、有创新性的东西。

## 如何做:

在Clojure中，我们使用Leiningen来创建新的项目。以下是创建一个名为"myapp"的新项目的命令：

```Clojure
lein new myapp
```

当我们运行这个命令时，将创建一个新的目录"myapp"，里面包含了开始一个新项目所需的所有文件。

## 深度解析

Clojure是 2007年由Rich Hickey发布的，它是一种运行在Java平台上的函数式编程语言。使用Leiningen启动新项目是最常用的方法，但也有其他的选择，比如 Boot 或者是直接使用 Clojure CLI工具。在内部，Leiningen实际上是用来处理项目的依赖关系，运行测试，打包代码以及其他相应的任务。

## 另请参阅

1. [Clojure官方网站](https://clojure.org/)
2. [Leiningen官方网站](https://leiningen.org/)
3. [函数式编程介绍](https://www.smashingmagazine.com/2014/07/dont-be-scared-of-functional-programming/)

请注意，这个指南的目标是给你一个关于如何在Clojure中启动新项目的基本了解。要想成为一名有效的Clojure开发人员，你需要深入学习这个语言的更多细微之处。