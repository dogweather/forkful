---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:31.665459-07:00
description: "\u5982\u4F55\u505A\uFF1A \u867D\u7136Go\u672C\u8EAB\u4E0D\u5305\u542B\
  \u5185\u7F6E\u7684REPL\uFF0C\u4F46\u793E\u533A\u5DF2\u7ECF\u521B\u5EFA\u4E86\u5982\
  `gore`\u8FD9\u6837\u7684\u5DE5\u5177\u6765\u586B\u8865\u8FD9\u4E00\u7A7A\u767D\u3002\
  \u9996\u5148\uFF0C\u901A\u8FC7\u8FD0\u884C\u4EE5\u4E0B\u547D\u4EE4\u5B89\u88C5`gore`\uFF1A\
  ."
lastmod: '2024-03-13T22:44:47.145216-06:00'
model: gpt-4-0125-preview
summary: "\u867D\u7136Go\u672C\u8EAB\u4E0D\u5305\u542B\u5185\u7F6E\u7684REPL\uFF0C\
  \u4F46\u793E\u533A\u5DF2\u7ECF\u521B\u5EFA\u4E86\u5982`gore`\u8FD9\u6837\u7684\u5DE5\
  \u5177\u6765\u586B\u8865\u8FD9\u4E00\u7A7A\u767D\u3002\u9996\u5148\uFF0C\u901A\u8FC7\
  \u8FD0\u884C\u4EE5\u4E0B\u547D\u4EE4\u5B89\u88C5`gore`\uFF1A."
title: "\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
weight: 34
---

## 如何做：
虽然Go本身不包含内置的REPL，但社区已经创建了如`gore`这样的工具来填补这一空白。首先，通过运行以下命令安装`gore`：

```
$ go get -u github.com/motemen/gore
```

安装完成后，通过在终端输入`gore`来启动`gore`：

```
$ gore
```

你应该看到一个提示符，准备接受Go命令。让我们试一个简单的例子：

```
gore> :import fmt
gore> fmt.Println("Hello, Go REPL!")
```

你会看到如下输出：

```
Hello, Go REPL!
```

变量和函数定义按预期工作。你可以声明一个函数：

```
gore> :import math
gore> areaCircle := func(radius float64) float64 {
...> return math.Pi * radius * radius
...> }
gore> fmt.Println("半径为4的圆的面积：", areaCircle(4))
```

并立即得到输出：

```
半径为4的圆的面积：50.26548245743669
```

## 深入探究：
REPL的概念是古老的，可以追溯到1960年代的Lisp机器，提供交互式编程体验。与Python或JavaScript等语言不同，Go在设计时没有包含REPL，而是专注于编译后的二进制文件，以提升性能和简化。这反映了Go的简洁哲学和其为可伸缩且可维护的软件而设计的目标。

然而，像`gore`或`goplay`这样的工具展示了Go社区在弥合这一差距上的资源性。这些工具动态地解析Go代码，并使用`go/eval`包或类似机制来实时执行代码，尽管与原生REPL环境相比存在一些限制。这些限制源于Go的类型系统和编译模型，这可能使得即时求值变得具有挑战性。

尽管REPL环境对于教育和快速测试非常有用，但Go生态系统通常倾向于对大多数开发任务使用传统的编译和运行流程。支持Go的IDE和编辑器，如Visual Studio Code或GoLand，提供了集成的测试和调试工具，大大减少了专业开发对REPL的需求。

然而，对于探索性编程、原型设计或学习等用途，像`gore`这样的REPL提供了一个有价值的选择，允许习惯于在其他语言中使用REPL的程序员在Go中享受类似的体验。
