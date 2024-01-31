---
title:                "写入标准错误"
date:                  2024-01-19
html_title:           "Arduino: 写入标准错误"
simple_title:         "写入标准错误"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
标准错误（stderr）是专用于输出错误信息的通道。程序员用它来分离正常输出和错误信息，便于调试和日志记录。

## How to: (如何操作：)
Haskell 使用 `System.IO` 模块中的 `hPutStrLn` 函数来写入标准错误。示例：

```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "这是一个错误消息"
```

执行以上代码，显示：

```
这是一个错误消息
```

注意：错误信息不会写入标准输出（stdout），而是stderr。

## Deep Dive (深入了解)
1. 历史背景：Haskell 的IO系统受到了Miranda等之前语言的影响，它坚持使用纯净的函数式编程范式。
2. 替代方案：除了`hPutStrLn`, 还可以使用`hPrint`函数输出错误。
3. 实现细节：`stderr`在`System.IO`中是`Handle`类型的值，它代表了操作系统的标准错误流。

## See Also (另请参阅)
- Haskell 语言官方文档: [System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
