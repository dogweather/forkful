---
title:                "使用交互式Shell（REPL）"
aliases:
- /zh/go/using-an-interactive-shell-repl.md
date:                  2024-02-03T18:10:31.665459-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用交互式Shell（REPL）"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？

交互式Shell，或者读取-求值-打印循环（REPL），让你可以实时地试验Go代码，执行命令并立即得到反馈。这种方法广泛用于学习、调试和原型设计，因为它绕过了传统的编辑-编译-运行周期，使得开发过程更快速和直观。

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
