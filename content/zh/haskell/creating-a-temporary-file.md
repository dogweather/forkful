---
title:    "Haskell: 创建临时文件"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 为什么要创建临时文件？

在进行Haskell编程时，有时会遇到需要暂时存储数据的情况。创建临时文件可以帮助我们在程序运行期间方便地在硬盘上存储临时数据，并且在程序结束后自动删除，这样可以节省内存空间。

## 如何创建临时文件？

下面我们将使用Haskell的`tempfile`库来展示如何创建临时文件。首先，我们需要导入`System.IO.Temp`模块：

```Haskell
import System.IO.Temp
```

接着，我们使用`withSystemTempFile`函数来创建临时文件：

```Haskell
withSystemTempFile "example" $ \path handle -> do
  -- 在此处可以进行对临时文件的操作
```

在上述例子中，我们使用`example`作为临时文件名，函数会自动生成临时文件并将其存储路径赋值给`path`变量。同时，我们也可以在此函数中执行对临时文件的操作，比如写入数据或读取数据。当程序结束时，`withSystemTempFile`函数会自动删除临时文件并关闭文件句柄。

## 深入了解创建临时文件

除了`withSystemTempFile`函数，`tempfile`库还提供了其他函数来创建临时文件，比如`withTempDirectory`、`withTempFile`等。这些函数都可以根据需要来创建不同名字和存储路径的临时文件。

除此之外，`tempfile`库还提供了一些选项来控制临时文件的操作，比如是否自动删除、创建临时文件时所使用的模板等。如果读者对这些选项感兴趣，可以查看[官方文档](https://hackage.haskell.org/package/temporary-1.3.0.1/docs/System-IO-Temp.html)来获取更详细的信息。

# 参考链接

- `tempfile`库官方文档：https://hackage.haskell.org/package/temporary-1.3.0.1/docs/System-IO-Temp.html
- Haskell官方文档：https://www.haskell.org/documentation/
- 了解更多Haskell编程知识：https://wiki.haskell.org/Haskell
- 使用Haskell创建临时文件：https://dev.to/aossama/how-to-create-temporary-files-and-folders-in-haskell-15k5