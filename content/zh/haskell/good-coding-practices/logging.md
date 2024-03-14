---
date: 2024-01-26 01:07:28.058659-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u65E5\u5FD7\u8BB0\u5F55\u672C\u8D28\u4E0A\
  \u662F\u5728\u4EE3\u7801\u4E2D\u7559\u4E0B\u4E00\u4E32\u9762\u5305\u5C51\uFF0C\u4EE5\
  \u8BB0\u5F55\u7684\u4E8B\u4EF6\u6216\u6D88\u606F\u7684\u5F62\u5F0F\uFF0C\u8FD9\u53EF\
  \u4EE5\u7528\u6765\u8FFD\u8E2A\u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u5728\u4EFB\u4F55\
  \u7ED9\u5B9A\u65F6\u523B\u90FD\u5728\u505A\u4EC0\u4E48\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u8C03\u8BD5\u95EE\u9898\u3001\u76D1\u63A7\u7CFB\u7EDF\
  \u6027\u80FD\uFF0C\u4EE5\u53CA\u51FA\u4E8E\u5B89\u5168\u548C\u5408\u89C4\u7684\u539F\
  \u56E0\u5BA1\u8BA1\u884C\u4E3A\u3002"
lastmod: '2024-03-13T22:44:47.823305-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u65E5\u5FD7\u8BB0\u5F55\u672C\u8D28\u4E0A\
  \u662F\u5728\u4EE3\u7801\u4E2D\u7559\u4E0B\u4E00\u4E32\u9762\u5305\u5C51\uFF0C\u4EE5\
  \u8BB0\u5F55\u7684\u4E8B\u4EF6\u6216\u6D88\u606F\u7684\u5F62\u5F0F\uFF0C\u8FD9\u53EF\
  \u4EE5\u7528\u6765\u8FFD\u8E2A\u4F60\u7684\u5E94\u7528\u7A0B\u5E8F\u5728\u4EFB\u4F55\
  \u7ED9\u5B9A\u65F6\u523B\u90FD\u5728\u505A\u4EC0\u4E48\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u8C03\u8BD5\u95EE\u9898\u3001\u76D1\u63A7\u7CFB\u7EDF\
  \u6027\u80FD\uFF0C\u4EE5\u53CA\u51FA\u4E8E\u5B89\u5168\u548C\u5408\u89C4\u7684\u539F\
  \u56E0\u5BA1\u8BA1\u884C\u4E3A\u3002"
title: "\u65E5\u5FD7\u8BB0\u5F55"
---

{{< edit_this_page >}}

## 什么是日志记录以及为什么要用日志记录？
在编程中，日志记录本质上是在代码中留下一串面包屑，以记录的事件或消息的形式，这可以用来追踪你的应用程序在任何给定时刻都在做什么。程序员这样做是为了调试问题、监控系统性能，以及出于安全和合规的原因审计行为。

## 如何进行日志记录：
在Haskell中，可以使用`monad-logger`或`hslogger`之类的库来实现日志功能。这里有一个使用`monad-logger`的简单示例：

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.IO.Class (liftIO)

logExample :: LoggingT IO ()
logExample = do
    logInfoN "开始应用程序..."
    liftIO $ putStrLn "正在进行一些关键工作..."
    logErrorN "哎呀！出了些问题。"

main :: IO ()
main = runStdoutLoggingT logExample

{- 示例输出
[信息] 开始应用程序...
正在进行一些关键工作...
[错误] 哎呀！出了些问题。
-}
```

这个简单的示例演示了你如何在代码中添加日志记录语句以了解运行时发生的情况。`logInfoN`和`logErrorN`分别用于记录信息性和错误消息。

## 深入了解：
日志记录已经从简单的打印语句发展到了复杂的日志记录框架。从历史上看，日志仅仅是输出到控制台或文件的文本，但现在它们包括了可以被各种工具解析和分析的结构化数据。

在Haskell中，日志记录可以以纯函数式的风格执行，涉及到显式传递日志操作，或者使用带有非纯粹性的单子上下文，日志记录器隐式地穿过计算过程。

例如，`hslogger`库与`monad-logger`相比更传统、更可变。`monad-logger`与单子栈集成，并在输出格式和控制方面提供了更多的灵活性。两个库都允许你设置日志级别，这有助于根据它们的重要性过滤日志消息。日志级别包括调试、信息、通知、警告、错误、严重、警报和紧急。

Haskell对日志记录的方法通常与其对类型安全和纯度的强调相一致。日志可以以这样的方式进行处理，即使日志记录失败，也不会因为Haskell强大的错误处理能力而导致主应用程序崩溃。

## 另请参阅：
- [Hackage上的`monad-logger`文档](https://hackage.haskell.org/package/monad-logger)
- [Hackage上的`hslogger`包](https://hackage.haskell.org/package/hslogger)
- [《Real World Haskell》第19章，关于错误处理](http://book.realworldhaskell.org/read/error-handling.html)
- [Haskell的日志门面（log-base）](https://hackage.haskell.org/package/log-base)
