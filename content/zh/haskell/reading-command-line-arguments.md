---
title:                "读取命令行参数"
html_title:           "Haskell: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么

在编写命令行程序时，读取命令行参数是一个非常常见的需求。通过阅读本文，您将学习如何使用Haskell语言来读取命令行参数，并且可以在您的项目中实践这一技巧。

# 如何操作

首先，我们需要导入System.Environment模块，它包含了许多有用的函数来操作命令行参数。

```Haskell
import System.Environment 
```

接下来，我们可以使用getArgs函数来读取命令行参数，并将它们存储为一个列表。

```Haskell
args <- getArgs 
```

通过索引访问列表中的参数。

```Haskell
let firstArg = args !! 0       -- 访问第一个参数
let secondArg = args !! 1      -- 访问第二个参数
```

如果需要，也可以将参数转换为其他类型，比如Int。

```Haskell
let num = read firstArg :: Int -- 将第一个参数转换为Int类型
```

现在，让我们来观察一下完整的代码，并打印出读取到的参数。

```Haskell
import System.Environment 

main = do 
  args <- getArgs 
  putStrLn "读取到的参数为：" 
  print args 
```

样例输出：

```
读取到的参数为：["Hello", "World"]
```

# 深入了解

除了getArgs函数，还有一些其他有用的函数可以帮助我们读取和处理命令行参数。比如，我们可以使用getProgName函数来获取当前执行的程序的名称。

另外，我们还可以使用System.Environment中的一些函数来操作环境变量。

# 参考链接

- https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html
- https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm

# 参见

- [Haskell官方文档](https://www.haskell.org/documentation/)