---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:17.616669-07:00
description: "\u5728 Haskell \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u5141\u8BB8\u7A0B\u5E8F\u533A\u5206\u5B83\u4EEC\u7684\u8F93\u51FA\uFF0C\u628A\u6B63\
  \u5E38\u7ED3\u679C\u548C\u9519\u8BEF\u6D88\u606F\u5206\u5F00\u3002\u8FD9\u5BF9\u4E8E\
  \u6307\u793A\u95EE\u9898\u548C\u8C03\u8BD5\u6765\u8BF4\u81F3\u5173\u91CD\u8981\uFF0C\
  \u800C\u4E0D\u4F1A\u8BA9\u6807\u51CA\u8F93\u51FA\uFF08stdout\uFF09\u53D8\u5F97\u6742\
  \u4E71\u65E0\u7AE0\uFF0Cstdout \u5F80\u5F80\u643A\u5E26\u7A0B\u5E8F\u7684\u4E3B\u8981\
  \u6570\u636E\u6216\u7ED3\u679C\u3002"
lastmod: '2024-03-13T22:44:47.834590-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Haskell \u4E2D\u5199\u5165\u6807\u51C6\u9519\u8BEF\uFF08stderr\uFF09\
  \u5141\u8BB8\u7A0B\u5E8F\u533A\u5206\u5B83\u4EEC\u7684\u8F93\u51FA\uFF0C\u628A\u6B63\
  \u5E38\u7ED3\u679C\u548C\u9519\u8BEF\u6D88\u606F\u5206\u5F00\u3002\u8FD9\u5BF9\u4E8E\
  \u6307\u793A\u95EE\u9898\u548C\u8C03\u8BD5\u6765\u8BF4\u81F3\u5173\u91CD\u8981\uFF0C\
  \u800C\u4E0D\u4F1A\u8BA9\u6807\u51CA\u8F93\u51FA\uFF08stdout\uFF09\u53D8\u5F97\u6742\
  \u4E71\u65E0\u7AE0\uFF0Cstdout \u5F80\u5F80\u643A\u5E26\u7A0B\u5E8F\u7684\u4E3B\u8981\
  \u6570\u636E\u6216\u7ED3\u679C\u3002"
title: "\u5199\u5165\u6807\u51C6\u9519\u8BEF"
weight: 25
---

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
