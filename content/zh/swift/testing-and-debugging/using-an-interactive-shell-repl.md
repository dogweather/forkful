---
title:                "在编程中使用交互式Shell（REPL）"
aliases:
- zh/swift/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:18:17.127928-07:00
model:                 gpt-4-0125-preview
simple_title:         "在编程中使用交互式Shell（REPL）"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
使用一个交互式Shell，或者一个读取-求值-打印循环（REPL），可以让你交互式地编码。程序员使用它来快速测试Swift代码片段、调试或学习该语言。

## 如何操作：
通过打开终端并运行`swift`来调用REPL。直接键入代码，然后按Enter运行它。这里有个示例：

```Swift
1> let greeting = "Hello, REPL!"
greeting: String = "Hello, REPL!"
2> print(greeting)
Hello, REPL!
```

使用`:quit`或`Control-D`退出。

## 深入了解
REPL的根源可以追溯到60年代的Lisp解释器。Swift的REPL建立在LLVM之上，一个强大的编译器框架，它提供的不仅仅是基本解释——它是一个完善的工具，具备自动完成、调试等功能。REPL非常适合学习或原型设计，但它不是一个独立的开发环境。有些人更喜欢在Xcode中使用Playgrounds来获得更图形化、基于文件的方法，而其他人则坚持传统的脚本编辑和运行。

在底层，Swift的REPL动态地将代码编译成机器语言并执行，这就是它相对快速的原因。它还可以访问任何编译过的Swift模块，甚至是C库，使它非常强大。不过，请注意，并非在REPL中的一切都能完美运行；一些Swift特性，特别是那些需要复杂项目设置或storyboard文件的，这里可能不适用。

## 另请参阅
- [Swift.org - 入门](https://www.swift.org/getting-started/#using-the-repl)
- 苹果公司的[Xcode Playgrounds简介](https://developer.apple.com/videos/play/wwdc2014/408/)
- [LLVM项目](https://llvm.org/)
