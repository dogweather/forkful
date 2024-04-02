---
date: 2024-01-20 17:52:40.340587-07:00
description: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u6307\u5728\u4EE3\u7801\u6267\
  \u884C\u65F6\u663E\u793A\u53D8\u91CF\u6216\u8BA1\u7B97\u7ED3\u679C\uFF0C\u5E2E\u52A9\
  \u5F00\u53D1\u8005\u4E86\u89E3\u7A0B\u5E8F\u8FD0\u884C\u72B6\u51B5\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5FEB\u901F\u5B9A\u4F4D\u95EE\u9898\u548C\
  \u7406\u89E3\u7A0B\u5E8F\u884C\u4E3A\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.675475-06:00'
model: gpt-4-1106-preview
summary: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA\u662F\u6307\u5728\u4EE3\u7801\u6267\
  \u884C\u65F6\u663E\u793A\u53D8\u91CF\u6216\u8BA1\u7B97\u7ED3\u679C\uFF0C\u5E2E\u52A9\
  \u5F00\u53D1\u8005\u4E86\u89E3\u7A0B\u5E8F\u8FD0\u884C\u72B6\u51B5\u3002\u7A0B\u5E8F\
  \u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\u5FEB\u901F\u5B9A\u4F4D\u95EE\u9898\u548C\
  \u7406\u89E3\u7A0B\u5E8F\u884C\u4E3A\u3002"
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

## What & Why? (是什么以及为什么？)
打印调试输出是指在代码执行时显示变量或计算结果，帮助开发者了解程序运行状况。程序员这么做是为了快速定位问题和理解程序行为。

## How to: (如何操作：)
Elm 提供 `Debug.log` 函数来打印值和调试信息。下面是如何使用它的例子：

```Elm
import Html

main =
  Html.text (Debug.log "The value is" "Hello, Elm!")
```

这行代码会输出：

```
"The value is": "Hello, Elm!"
```

## Deep Dive (深入了解)
Elm 的调试功能在历史上有很大改进，尤其是跟其他语言相比。最开始，Elm 语言并不支持控制台输出，但随着社区的发展，`Debug.log` 被添加进来，使得开发过程中的数据跟踪变得容易。尽管如此，Elm 对调试有着独到的理念，它鼓励开发者使用类型系统防止错误，而不是依赖大量的打印输出。除了 `Debug.log`，Elm 还有 `Debug.todo` 这类工具，但它们不应该出现在生产代码中。在 Elm 0.19 版本中，打印到控制台的能力进一步被限制，反映了使语言更纯净、更可靠的目标。

## See Also (另请参看)
- Elm 官方文档中的 `Debug` 模块: [Elm Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- 关于 Elm 调试的社区讨论: [Elm Discourse](https://discourse.elm-lang.org/)
- 一个介绍 Elm 调试哲学的博客文章: [Elm Debugging Philosophy](https://elm-lang.org/news/the-perfect-bug-report)
