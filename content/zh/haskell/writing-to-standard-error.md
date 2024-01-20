---
title:                "标准错误写作"
html_title:           "Haskell: 标准错误写作"
simple_title:         "标准错误写作"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 什么是标准错误？为什么程序员要这么做？

标准错误是将错误信息输出到屏幕或日志文件中，而不是常规输出流。程序员通常会将错误信息写入标准错误，这样可以更容易地检测和解决程序运行中的问题。这也是一种良好的代码习惯，可以帮助提高代码的健壮性和可维护性。

# 如何实现？

下面是一个简单的Haskell代码示例，演示如何将错误信息输出到标准错误流中：

```Haskell
import System.IO

main = do
  hPutStrLn stderr "这是一个错误信息"
```

运行以上代码，我们可以在命令行界面中看到“这是一个错误信息”这个错误提示。这样，我们就可以及时发现并定位问题所在了。

# 深入探讨

## 历史背景

在早期的计算机系统中，程序的输出通常被托管到终端设备（通常是硬件打印机），而错误信息则输出到错误流中。随着计算机系统的发展，这种习惯被保留下来，并成为了一种标准做法。

## 其他选择

除了将错误消息输出到标准错误流中，程序员也可以选择将其写入到日志文件中。不过，将错误信息输出到标准错误流更直接，也更容易被发现。

## 实现细节

在Haskell中，我们可以使用```System.IO```模块中的```hPutStrLn```来将错误消息写入标准错误流中。另外，也可以使用Haskell标准库中提供的```hPutStr```和```hPutStrLn```函数来实现相同的功能。

# 参考资料

- [Hackage文档 - System.IO模块](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [Haskell入门教程](https://www.haskell.org/tutorial/io.html)