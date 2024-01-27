---
title:                "使用正则表达式"
date:                  2024-01-19
html_title:           "C: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？

正则表达式是一种强大的文本模式匹配和查找工具。程序员用它来快速处理复杂的字符串操作，进行搜索、替换和数据校验。简洁、高效、灵活。

## How to: 如何做

```Haskell
import Text.Regex.TDFA ((=~))

-- 示例：查找单词
let example = "Hello, Haskell 2023!"
let pattern = "\\b[a-zA-Z]+\\b" :: String
putStrLn $ "Matching words: " ++ show (getAllTextMatches (example =~ pattern :: AllTextMatches [] String))

-- 输出：
-- Matching words: ["Hello", "Haskell"]
```

```Haskell
-- 示例：替换文字
import Text.Regex.TDFA ((=~), (=~~))

replaceText :: String -> String -> String -> String
replaceText text pattern replacement = text =~~ pattern >>= return . flip (maybe text) replacement

main :: IO ()
main = do
  let text = "Hello, World!"
  let pattern = "World"
  let replacement = "Haskell"
  putStrLn (replaceText text pattern replacement)

-- 输出:
-- Hello, Haskell!
```

## Deep Dive 深入了解

- 历史情况：正则表达式起源于20世纪50年代的神经心理学研究。在软件界，它自Unix时代早期就开始使用，并在Perl编程语言中得到普及。
- 替代品：Haskell中的替代方案包括Parser组合子库如Parsec，它提供了更为细致的控制和更强的解析能力，适合复杂语法分析。
- 实现细节：Haskell的正则表达式依靠第三方库，如`regex-tdfa`，提供Posix兼容正则表达式支持。它利用Tagged DFA算法实现，性能在大多数场景下足够好。

## See Also 相关链接

- Haskell `regex-tdfa`库文档：[Hackage: regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
- 廖雪峰的正则表达式教程（中文介绍正则表达式）：[廖雪峰的官方网站](https://www.liaoxuefeng.com/wiki/1016959663602400)
