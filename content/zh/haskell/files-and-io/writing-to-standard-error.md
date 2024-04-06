---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:17.616669-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728 Haskell \u4E2D\uFF0C\u4F7F\u7528\u57FA\
  \u7840\u5E93\u7684 `System.IO` \u6A21\u5757\u5411 stderr \u5199\u5165\u76F8\u5F53\
  \u7B80\u5355\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\u6765\u6F14\
  \u793A\uFF1A."
lastmod: '2024-04-05T22:38:46.995411-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u505A\uFF1A \u5728 Haskell \u4E2D\uFF0C\u4F7F\u7528\u57FA\u7840\
  \u5E93\u7684 `System.IO` \u6A21\u5757\u5411 stderr \u5199\u5165\u76F8\u5F53\u7B80\
  \u5355\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\u6765\u6F14\u793A\
  \uFF1A."
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
