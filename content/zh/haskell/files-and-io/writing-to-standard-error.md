---
title:                "写入标准错误"
aliases:
- /zh/haskell/writing-to-standard-error/
date:                  2024-02-03T19:33:17.616669-07:00
model:                 gpt-4-0125-preview
simple_title:         "写入标准错误"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 Haskell 中写入标准错误（stderr）允许程序区分它们的输出，把正常结果和错误消息分开。这对于指示问题和调试来说至关重要，而不会让标凊输出（stdout）变得杂乱无章，stdout 往往携带程序的主要数据或结果。

## 如何做：
在 Haskell 中，使用基础库的 `System.IO` 模块向 stderr 写入相当简单。下面是一个基本示例来演示：

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "这是一条错误信息。"
```

此程序对 stderr 的输出将是：

```
这是一条错误信息。
```

如果你在更复杂的应用程序中工作，或者你需要更好地控制日志记录（包括错误），你可能会选择一个第三方库。一个受欢迎的选择是 `monad-logger`，它与 Haskell 编程的 `mtl` 风格集成。这是使用 `monad-logger` 的一个小片段：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "这是使用 monad-logger 的错误信息。"
```

当运行时，`monad-logger` 版本同样输出一个错误信息，但它配备了更多上下文，如时间戳或日志级别，这取决于配置：

```
[Error] 这是使用 monad-logger 的错误信息。
```

这两种方法都实现了向 stderr 写入的目的，选择主要取决于你的应用程序的复杂性和需求。
