---
title:    "Haskell: 读取命令行参数"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么？

命令行参数是在Haskell编程中经常使用的重要概念。通过读取命令行参数，我们可以让我们的程序更具有灵活性，让它们能够以不同的方式运行。阅读本文可以帮助您学习如何在Haskell中读取命令行参数，并将其应用到您的编程项目中。

## 如何

要读取命令行参数，我们需要使用一个名为"System.Environment"的Haskell模块。首先，我们需要在程序的顶部导入这个模块，如下所示：

```Haskell
import System.Environment
```

接下来，我们可以使用"getArgs"函数来读取命令行参数。"getArgs"函数将返回一个名为"IO [String]"的操作，其中"IO"表示它包含"IO"操作，并且返回值是一个字符串列表。因此，我们可以通过以下方式来读取命令行参数：

```Haskell
args <- getArgs
```

这个"args"变量就是我们读取到的命令行参数列表。我们可以通过使用"!!"运算符来访问特定的参数，该运算符接受一个索引值作为参数。例如，如果我们想要访问第一个命令行参数，我们可以使用"args !! 0"来获取它。

下面是一个完整的示例程序，它将打印出用户提供的第一个和第二个命令行参数：

``` Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("The first argument is: " ++ args !! 0)
    putStrLn ("The second argument is: " ++ args !! 1)
```

如果我们在命令行中使用"runhaskell"命令运行这个程序，并且提供两个参数，比如"abc"和"123"，那么程序将会输出以下内容：

``` 
The first argument is: abc
The second argument is: 123
```

## 深入了解

除了"getArgs"函数，"System.Environment"模块还提供了其他一些函数来帮助我们处理命令行参数。例如，我们可以使用"getProgName"函数来获取程序的名称，使用"getEnv"函数来获取环境变量的值，使用"lookupEnv"函数来查找特定的环境变量，等等。您可以在官方文档中找到更多关于这些函数的信息。

## 参考链接

- [Official documentation for "System.Environment" module](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [Haskell Tutorial - Getting Command Line Arguments](https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm)
- [Haskell入门教程-获取命令行参数](https://studygolang.com/articles/27729)
- [Haskell中使用命令行参数](https://www.yuque.com/bhdy/blog/xd1zft)

## 请参阅

- [Haskell编程指南](https://www.haskell.org/tutorial/)
- [Haskell命令行工具集合](https://github.com/cli-guidelines/cli-guidelines)
- [Haskell编程入门](https://learnxinyminutes.com/docs/zh-cn/haskell-cn/)