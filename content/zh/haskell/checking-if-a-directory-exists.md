---
title:    "Haskell: 检查目录是否存在"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么会要检查目录是否存在

当我们在编写Haskell程序时，可能会遇到需要访问或操作某个目录的情况。在这种情况下，我们需要先检查该目录是否存在，以避免出现错误。因此，了解如何检查目录是否存在是非常重要的。

# 如何进行检查

我们可以使用Haskell的System.Directory模块来检查目录是否存在。首先，我们需要导入该模块，然后使用`doesDirectoryExist`函数来查询目录是否存在。下面是一个简单的例子：

```Haskell
import System.Directory

directoryExists <- doesDirectoryExist "path/to/directory"

if directoryExists
    then putStrLn "目录存在！"
    else putStrLn "目录不存在！"
```

运行以上代码，如果路径所指示的目录存在，将会得到"目录存在！"的输出；否则，将会得到"目录不存在！"的输出。

# 深入探讨

`doesDirectoryExist`函数实际上是调用了`getAccessTime`函数来获取目录的访问时间。如果目录不存在，将会抛出一个IO错误。因此，我们可以根据这个IO错误来判断目录是否存在。

另外，我们也可以使用`listDirectory`函数来遍历目录中的文件和子目录，从而进一步操作目录中的内容。

# 参考链接

- [Haskell官方文档](https://www.haskell.org/cabal/users-guide/installing-packages.html)
- [Haskell中文社区](https://www.haskellchina.org/)
- [Haskell教程](https://www.tutorialspoint.com/haskell/index.htm)

# 查看也可

- [如何在Haskell中创建和操作目录](https://www.example.com/create-and-manipulate-directories-in-haskell)
- [掌握Haskell中的IO错误处理](https://www.example.com/error-handling-in-haskell)
- [Haskell中的文件操作指南](https://www.example.com/file-handling-in-haskell)