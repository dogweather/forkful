---
date: 2024-01-20 17:51:16.684518-07:00
description: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u6216\u8868\u8FBE\
  \u5F0F\u7684\u503C\u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\
  \u7A0B\u5E8F\u5458\u7528\u5B83\u662F\u4E3A\u4E86\u52A8\u6001\u6784\u5EFA\u5B57\u7B26\
  \u4E32\uFF0C\u63D0\u9AD8\u4EE3\u7801\u7684\u53EF\u8BFB\u6027\u548C\u7075\u6D3B\u6027\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.800209-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u63D2\u503C\u662F\u5C06\u53D8\u91CF\u6216\u8868\u8FBE\
  \u5F0F\u7684\u503C\u5D4C\u5165\u5230\u5B57\u7B26\u4E32\u4E2D\u7684\u8FC7\u7A0B\u3002\
  \u7A0B\u5E8F\u5458\u7528\u5B83\u662F\u4E3A\u4E86\u52A8\u6001\u6784\u5EFA\u5B57\u7B26\
  \u4E32\uFF0C\u63D0\u9AD8\u4EE3\u7801\u7684\u53EF\u8BFB\u6027\u548C\u7075\u6D3B\u6027\
  \u3002"
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## What & Why? (是什么以及为什么？)
字符串插值是将变量或表达式的值嵌入到字符串中的过程。程序员用它是为了动态构建字符串，提高代码的可读性和灵活性。

## How to: (如何操作：)
Haskell 中可以用多种方式插值字符串，但让我们看一个简单例子：使用 `printf` 函数。

```Haskell
import Text.Printf (printf)

name = "张三"
age = 28

main = printf "Hello, %s! You are %d years old.\n" name age
```

输出：

```
Hello, 张三! You are 28 years old.
```

## Deep Dive (深入探究)
过去，Haskell 没有内建的字符串插值功能。`printf` 来自 C 语言家族，在 Haskell 中通过 `Text.Printf` 模块提供类似功能。还有其他库，比如 `interpolate` 和 `string-interpolate`，它们使得插值更加方便。

例如，`string-interpolate` 库允许您这样写：

```Haskell
{-# LANGUAGE QuasiQuotes #-}
import Data.String.Interpolate (i)

name = "李四"
occupation = "程序员"

main = putStrLn [i|你好，#{name}! 你是一名#{occupation}.|]
```

`QuasiQuotes` 让 Haskell 支持自定义插值语法。上面的代码也需要添加包依赖和适当配置。

其他根本性的方法包括字符串连接或使用 `++` 运算符拼接较简单的情况。例如：

```Haskell
main = putStrLn ("你好，" ++ name ++ "! 你是一名" ++ occupation ++ ".")
```

这种方式相比插值来说，对于复杂字符串会变得笨拙且不易读。

实现细节上，使用插值函数通常涉及解析字符串并在运行时替换标记，这可能比纯粹的字符串连接稍微慢一点，但在正常使用情况下差别不大。

## See Also (另请参阅)
- Haskell `Text.Printf` 文档：https://hackage.haskell.org/package/base-4.16.1.0/docs/Text-Printf.html
- `string-interpolate` 库：https://hackage.haskell.org/package/string-interpolate
- 关于 `QuasiQuotes` 的更多信息：https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#quasi-quotation
