---
aliases:
- /zh/haskell/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:26.316284-07:00
description: "\u5C06\u5B57\u7B26\u4E32\u7684\u9996\u5B57\u6BCD\u8F6C\u6362\u4E3A\u5927\
  \u5199\uFF0C\u540C\u65F6\u786E\u4FDD\u5176\u4F59\u5B57\u6BCD\u4FDD\u6301\u5C0F\u5199\
  \uFF0C\u8FD9\u4E00\u8FC7\u7A0B\u88AB\u79F0\u4E3A\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u683C\u5F0F\
  \u5316\u8F93\u51FA\u3001\u9075\u5FAA\u6587\u672C\u4E2D\u7684\u8BED\u6CD5\u6B63\u786E\
  \u6027\u6216\u6539\u5584\u751F\u6210\u6570\u636E\u7684\u53EF\u8BFB\u6027\u3002"
lastmod: 2024-02-18 23:08:59.155409
model: gpt-4-0125-preview
summary: "\u5C06\u5B57\u7B26\u4E32\u7684\u9996\u5B57\u6BCD\u8F6C\u6362\u4E3A\u5927\
  \u5199\uFF0C\u540C\u65F6\u786E\u4FDD\u5176\u4F59\u5B57\u6BCD\u4FDD\u6301\u5C0F\u5199\
  \uFF0C\u8FD9\u4E00\u8FC7\u7A0B\u88AB\u79F0\u4E3A\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\
  \u5927\u5199\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u683C\u5F0F\
  \u5316\u8F93\u51FA\u3001\u9075\u5FAA\u6587\u672C\u4E2D\u7684\u8BED\u6CD5\u6B63\u786E\
  \u6027\u6216\u6539\u5584\u751F\u6210\u6570\u636E\u7684\u53EF\u8BFB\u6027\u3002"
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将字符串的首字母转换为大写，同时确保其余字母保持小写，这一过程被称为字符串首字母大写。程序员这样做是为了格式化输出、遵循文本中的语法正确性或改善生成数据的可读性。

## 如何操作：
在 Haskell 中，你可以使用标准库来实现字符串首字母大写，无需任何第三方库。

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- 示例用法：
main = putStrLn $ capitalize "hello world"
```

输出：
```
Hello world
```

对于更复杂的场景或为了使用上的便利，你可能会想要使用第三方库，如 `text`，它因在 Haskell 中高效的字符串操作而受欢迎。

首先，你需要将 `text` 添加到项目的依赖中。然后，你可以按如下方式使用其函数来实现字符串首字母大写：

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- 使用 text 库的示例用法：
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

输出：
```
Hello world
```

这两个例子都展示了在 Haskell 中使用或不使用第三方库来实现字符串首字母大写的简单而有效的方法。
