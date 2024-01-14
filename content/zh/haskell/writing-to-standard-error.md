---
title:    "Haskell: 写入标准错误"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## 为什么
有时候我们在编程中可能会遇到一些错误。而这些错误信息可能并不是我们希望用户看到的，因此，我们需要一种方式来把这些错误信息输出到一个特定的地方，而不是直接显示给用户。这就是为什么我们需要写入标准错误输出流的原因。

## 如何做
在Haskell中，我们可以使用`stderr`函数来把错误信息输出到标准错误流。下面是一个示例代码：

```Haskell
import System.IO

main = do
  hPutStrLn stderr "这是一个错误信息。"
```

运行这段代码，我们会发现在控制台并没有打印出这个错误信息，而是在另一个地方。这就是标准错误流的作用。

## 深入了解
除了使用`hPutStrLn`函数，我们还可以使用`hPutStr`、`hPutChar`等函数来输出错误信息。同时，我们也可以使用`stderr`函数来获取标准错误流，然后进行一些其他操作。

例如，我们可以使用`withFile`函数来将错误信息输出到一个文件中，而不是直接显示在控制台。代码示例如下：

```Haskell
import System.IO

main = do
  withFile "error.txt" WriteMode $ \handle -> do
    hPutStrLn handle "这是一个错误信息。"
```

在这个例子中，我们使用`withFile`函数来创建一个文件句柄，然后将错误信息通过`hPutStrLn`函数输出到文件中。这样就能更方便地管理错误信息了。

## 参考链接
- [Haskell标准库文档](https://hackage.haskell.org/package/base/docs/System-IO.html#g:3)
- [Haskell错误处理](https://wiki.haskell.org/Error)
- [Learn You a Haskell错误处理小节](http://learnyouahaskell.com/error-handling)