---
title:    "Haskell: 读取命令行参数"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 为什么

编程语言Haskell是一种广受欢迎的函数式编程语言，在处理命令行参数时也有其独特的优势。通过阅读这篇博文，你将学习如何在Haskell中读取命令行参数，从而更有效地处理命令行输入。

## 如何操作

在Haskell中读取命令行参数非常简单。首先，你需要导入System.Environment模块来访问命令行参数函数。接下来，使用getArgs函数来接收命令行参数，并将其存储在一个列表中。例如：

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Welcome to Haskell, " ++ head args ++ "!")
```

假设你的程序命名为hello.hs，当你在命令行中执行以下命令时：

```bash
$ runhaskell hello.hs Tony
```

它将输出：

```bash
Welcome to Haskell, Tony!
```

## 深入探索

除了使用getArgs函数来读取命令行参数外，Haskell还提供了其他几种方法。例如，你可以使用getProgName函数来获取当前程序的名称，使用getEnv函数来读取环境变量，使用getArgsAndInitialize函数来读取命令行参数并初始化一个GUI应用程序等等。在学习Haskell的过程中，掌握这些技巧将会对你有很大的帮助。

## 参考资料

- [Haskell官方文档：System.Environment模块](https://www.haskell.org/documentation/#system-environment-module)
- [Haskell命令行参数教程](https://wiki.haskell.org/Command_line_argument)
- [Haskell命令行参数实用例子](https://denisrosset.com/2017/04/29/command-line-arguments-haskell.html)

## 参见

- [Haskell官网](https://www.haskell.org/)
- [Haskell中文社区](https://haskell.ctolib.com/)