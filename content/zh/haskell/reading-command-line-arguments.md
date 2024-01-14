---
title:                "Haskell: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Haskell程序设计博客文章：如何读取命令行参数

## 为什么

读取命令行参数是在Haskell编程中非常常见的操作。它允许你从用户输入中获取信息，使得你的程序更加灵活和交互性。因此，通过学习如何读取命令行参数，你可以提升你的Haskell编程技能并开发更加强大的程序。

## 如何

Haskell提供了一个`System.Environment`模块来帮助我们读取命令行参数。首先，我们需要导入这个模块：

```Haskell
import System.Environment
```

接下来，我们可以使用`getArgs`函数来获取命令行参数列表：

```Haskell
args <- getArgs
```

`args`将会是一个字符串的列表，每个字符串都代表一个命令行参数。让我们来看一个例子：

假设我们有一个程序叫做`hello.hs`，它接受两个命令行参数：名字和年龄。我们可以通过以下方式运行程序：

```bash
runhaskell hello.hs Tom 25
```

那么在我们的程序中，我们可以这样获取命令行参数：

```Haskell
import System.Environment

main = do
  args <- getArgs
  let name = args !! 0 -- 获取第一个参数，即名字
      age = read (args !! 1) :: Int -- 获取第二个参数，即年龄，并将其转换为Int类型
  putStrLn ("Hello " ++ name ++ "! You are " ++ show age ++ " years old.")
```

运行后，你会得到输出："Hello Tom! You are 25 years old."

需要注意的是，命令行参数是从索引0开始的，所以我们需要使用`!!`操作符来获取指定位置的参数。

## 深入了解

除了`getArgs`函数外，`System.Environment`模块还提供了其他一些方法帮助我们读取命令行参数。比如，`getProgName`函数可以获取当前程序的名称，`getEnv`函数可以用来获取环境变量。如果你想了解更多细节，可以查看[Haskell官方文档](https://www.haskell.org/cabal/users-guide/developing-packages.html#buildinfo-fields)。

## 参考文献

- [Haskell官方文档](https://www.haskell.org/cabal/users-guide/developing-packages.html#buildinfo-fields)
- [Learn You a Haskell for Great Good! - Command line arguments](http://learnyouahaskell.com/input-and-output#command-line-arguments)
- [Real World Haskell - Command Line Arguments](http://book.realworldhaskell.org/read/io.html#id617989)

# 参见

- [如何创建一个简单的Haskell程序](https://www.example.com/how-to-create-a-simple-haskell-program)
- [使用Haskell编写命令行工具的最佳实践](https://www.example.com/best-practices-for-writing-command-line-tools-in-haskell)