---
date: 2024-01-20 17:38:31.760089-07:00
description: "\u4EC0\u4E48\u548C\u4E3A\u4EC0\u4E48\uFF1F \u628A\u5B57\u7B26\u4E32\
  \ (string) \u8F6C\u6362\u4E3A\u5C0F\u5199\u610F\u5473\u7740\u5C06\u6587\u672C\u4E2D\
  \u7684\u6240\u6709\u5927\u5199\u5B57\u6BCD\u6539\u6210\u5C0F\u5199\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u901A\u5E38\u8FD9\u6837\u505A\u4EE5\u5B9E\u73B0\u4E0D\u533A\u5206\
  \u5927\u5C0F\u5199\u7684\u6570\u636E\u6BD4\u8F83\u6216\u641C\u7D22\u3002 \u5728\
  Haskell\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`Data.Char`\u6A21\u5757\u4E2D\u7684\
  `toLower`\u51FD\u6570\u6765\u8F6C\u6362\u5B57\u7B26\u4E32\u4E2D\u7684\u6BCF\u4E2A\
  \u5B57\u7B26\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u7B80\u5355\u7684\u4F8B\u5B50\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.800898-06:00'
model: gpt-4-1106-preview
summary: "\u628A\u5B57\u7B26\u4E32 (string) \u8F6C\u6362\u4E3A\u5C0F\u5199\u610F\u5473\
  \u7740\u5C06\u6587\u672C\u4E2D\u7684\u6240\u6709\u5927\u5199\u5B57\u6BCD\u6539\u6210\
  \u5C0F\u5199\u5F62\u5F0F\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u8FD9\u6837\u505A\u4EE5\
  \u5B9E\u73B0\u4E0D\u533A\u5206\u5927\u5C0F\u5199\u7684\u6570\u636E\u6BD4\u8F83\u6216\
  \u641C\u7D22."
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## 什么和为什么？
把字符串 (string) 转换为小写意味着将文本中的所有大写字母改成小写形式。程序员通常这样做以实现不区分大小写的数据比较或搜索。

## How to:


## 如何操作：
在Haskell中，你可以使用`Data.Char`模块中的`toLower`函数来转换字符串中的每个字符。以下是一个简单的例子：

```Haskell
import Data.Char (toLower)

-- 将字符串转换为小写
toLowerCase :: String -> String
toLowerCase = map toLower

main :: IO ()
main = putStrLn $ toLowerCase "Hello, World!"
```

当你运行这个程序，输出会是：

```
hello, world!
```

## Deep Dive


## 深入了解
转换字符串到小写在历史上一直是文本处理中的一个基本任务。在早期编程时期，这甚至成为了一大挑战，因为字符编码和国际化标准尚未统一。

除了`toLower`函数，另外有一些库提供了更复杂的用法，例如处理Unicode字符等复杂情况。例如，`text`和`case-insensitive`库提供这样的功能。

`toLower`函数本身很简单，它只对ASCII范围内的字符有效。当处理Unicode字符时，更复杂的规则通常需要考虑，如特殊情况处理和语言特定规则。

## See Also


## 另请参阅
- Haskell `Data.Char` 模块文档：https://hackage.haskell.org/package/base/docs/Data-Char.html
- `text` 库：https://hackage.haskell.org/package/text
- `case-insensitive` 库：https://hackage.haskell.org/package/case-insensitive
