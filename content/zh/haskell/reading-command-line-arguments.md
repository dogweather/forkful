---
title:                "Haskell: 读取命令行参数"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

在编写Haskell程序时，可能会需要使用命令行参数来传递输入信息。了解如何读取和使用命令行参数可以让你的程序更加灵活和通用，提高编程的效率。

## 如何做

使用Haskell内置的`getArgs`方法可以读取命令行参数。首先，需要在程序的顶部引入`System.Environment`模块，然后在需要读取参数的地方使用`getArgs`方法。下面是一个示例代码：

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("你输入的参数是：" ++ show args)
```

这段代码中，我们使用`putStrLn`方法将读取到的参数打印出来。然后，在命令行中执行编译后的程序并输入参数，就可以看到输出的结果了。假设程序的名字是`sample`，那么在命令行中可以这样运行：

```bash
./sample Hello Haskell
```

输出的结果将是：

```text
你输入的参数是：["Hello", "Haskell"]
```

## 深入了解

除了使用`getArgs`方法外，还可以使用`getProgName`方法获取当前程序的名字，以及使用`withArgs`方法改变命令行参数。此外，Haskell还提供了一些其他的方法来处理命令行参数，如`getEnv`方法可以读取环境变量，而`getContents`方法可以读取标准输入。深入了解这些方法可以帮助你更好地处理命令行参数。

## 参考链接

- [Haskell命令行参数文档](https://www.haskell.org/hoogle/?hoogle=getArgs)
- [使用Haskell处理命令行参数](https://wiki.haskell.org/Handling_command-line_arguments)
- [Haskell文档：System.Environment](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)