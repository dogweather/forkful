---
title:                "字符串大写化"
aliases:
- zh/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:26.316284-07:00
model:                 gpt-4-0125-preview
simple_title:         "字符串大写化"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
