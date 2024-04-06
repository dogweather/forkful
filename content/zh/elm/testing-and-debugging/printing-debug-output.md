---
date: 2024-01-20 17:52:40.340587-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) Elm \u63D0\u4F9B `Debug.log`\
  \ \u51FD\u6570\u6765\u6253\u5370\u503C\u548C\u8C03\u8BD5\u4FE1\u606F\u3002\u4E0B\
  \u9762\u662F\u5982\u4F55\u4F7F\u7528\u5B83\u7684\u4F8B\u5B50\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:47.993241-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) Elm \u63D0\u4F9B `Debug.log` \u51FD\u6570\
  \u6765\u6253\u5370\u503C\u548C\u8C03\u8BD5\u4FE1\u606F\u3002\u4E0B\u9762\u662F\u5982\
  \u4F55\u4F7F\u7528\u5B83\u7684\u4F8B\u5B50\uFF1A."
title: "\u6253\u5370\u8C03\u8BD5\u8F93\u51FA"
weight: 33
---

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
