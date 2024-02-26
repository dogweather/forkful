---
date: 2024-01-26 04:13:42.582452-07:00
description: "\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF08REPL\uFF09\u662F\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4EA4\u4E92\u5F0F\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\
  \u63A5\u6536\u7528\u6237\u7684\u5355\u4E00\u8F93\u5165\uFF0C\u5BF9\u5176\u8FDB\u884C\
  \u6C42\u503C\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\u56DE\u7ED9\u7528\u6237\u3002Elm\u7A0B\
  \u5E8F\u5458\u4F7F\u7528REPL\u8FDB\u884C\u5FEB\u901F\u5B9E\u9A8C\u3001\u8C03\u8BD5\
  \u6216\u5B66\u4E60\u8BE5\u8BED\u8A00\u3002"
lastmod: '2024-02-25T18:49:45.239349-07:00'
model: gpt-4-0125-preview
summary: "\u8BFB\u53D6-\u6C42\u503C-\u6253\u5370\u5FAA\u73AF\uFF08REPL\uFF09\u662F\
  \u4E00\u4E2A\u7B80\u5355\u7684\u4EA4\u4E92\u5F0F\u7F16\u7A0B\u73AF\u5883\uFF0C\u5B83\
  \u63A5\u6536\u7528\u6237\u7684\u5355\u4E00\u8F93\u5165\uFF0C\u5BF9\u5176\u8FDB\u884C\
  \u6C42\u503C\uFF0C\u5E76\u5C06\u7ED3\u679C\u8FD4\u56DE\u7ED9\u7528\u6237\u3002Elm\u7A0B\
  \u5E8F\u5458\u4F7F\u7528REPL\u8FDB\u884C\u5FEB\u901F\u5B9E\u9A8C\u3001\u8C03\u8BD5\
  \u6216\u5B66\u4E60\u8BE5\u8BED\u8A00\u3002"
title: "\u5728\u7F16\u7A0B\u4E2D\u4F7F\u7528\u4EA4\u4E92\u5F0FShell\uFF08REPL\uFF09"
---

{{< edit_this_page >}}

## 什么和为什么？
读取-求值-打印循环（REPL）是一个简单的交互式编程环境，它接收用户的单一输入，对其进行求值，并将结果返回给用户。Elm程序员使用REPL进行快速实验、调试或学习该语言。

## 如何操作：
Elm自身并不内置REPL。但是，安装Elm之后，您可以通过命令行使用`elm repl`来启动一个Elm会话。

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

在这个会话中，导入List函数后，我们将列表中的数字翻倍，立即得到了结果。

## 深入了解
与Python或JavaScript等其他一些语言的REPL相比，Elm的REPL可能看起来有限制，因为Elm是一种编译型语言，专注于生成Web应用程序。从历史上看，Elm专注于完整的应用程序，而不是脚本编写或shell交互。

Elm REPL的替代品包括`elm-live`和像Ellie这样的在线编辑器，在这些编辑器中，您可以实时在浏览器中看到代码的变化。

就实现而言，Elm REPL在后台将Elm代码片段编译成JavaScript，允许您交互式地运行Elm。这与解释型语言的REPL不同，后者不需要这个编译步骤。Elm REPL也被简化，以保持核心语言轻量级和专注。

## 另请参见
- Elm关于互动性的官方指南：https://guide.elm-lang.org/interop/
- Ellie, 一个在线Elm游乐场：https://ellie-app.com/new
- `elm-live`，一个灵活的Elm开发服务器：https://www.elm-live.com/
