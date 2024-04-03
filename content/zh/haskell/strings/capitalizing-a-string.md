---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:26.316284-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 Haskell \u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528\u6807\u51C6\u5E93\u6765\u5B9E\u73B0\u5B57\u7B26\u4E32\u9996\u5B57\
  \u6BCD\u5927\u5199\uFF0C\u65E0\u9700\u4EFB\u4F55\u7B2C\u4E09\u65B9\u5E93\u3002"
lastmod: '2024-03-13T22:44:47.797173-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Haskell \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\u6807\u51C6\u5E93\
  \u6765\u5B9E\u73B0\u5B57\u7B26\u4E32\u9996\u5B57\u6BCD\u5927\u5199\uFF0C\u65E0\u9700\
  \u4EFB\u4F55\u7B2C\u4E09\u65B9\u5E93."
title: "\u5B57\u7B26\u4E32\u5927\u5199\u5316"
weight: 2
---

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
