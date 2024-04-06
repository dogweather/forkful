---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:20.469295-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u8981\u5728 Haskell \u4E2D\u89E3\u6790\
  \ HTML\uFF0C\u6211\u4EEC\u5C06\u4F7F\u7528 `tagsoup` \u5E93\uFF0C\u56E0\u5176\u7B80\
  \u5355\u548C\u7075\u6D3B\u6027\u800C\u9009\u7528\u3002\u9996\u5148\uFF0C\u901A\u8FC7\
  \u5728\u9879\u76EE\u7684 cabal \u6587\u4EF6\u4E2D\u6DFB\u52A0 `tagsoup` \u6216\u8FD0\
  \u884C `cabal install tagsoup` \u6765\u786E\u4FDD\u5B89\u88C5\u8BE5\u5E93\u3002"
lastmod: '2024-04-05T21:53:48.125674-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何进行：
要在 Haskell 中解析 HTML，我们将使用 `tagsoup` 库，因其简单和灵活性而选用。首先，通过在项目的 cabal 文件中添加 `tagsoup` 或运行 `cabal install tagsoup` 来确保安装该库。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- 示例 HTML 以作演示
let sampleHtml = "<html><body><p>Learn Haskell!</p><a href='http://example.com'>点击这里</a></body></html>"

-- 解析 HTML 并过滤链接 (a 标签)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- 打印提取的链接
print links
```

示例输出：
```plaintext
["http://example.com"]
```

对于更复杂的 HTML 解析需求，可以考虑使用 `pandoc` 库，特别是当你处理文档转换时。它异常灵活，但带有更多的复杂性：

```haskell
import Text.Pandoc

-- 假设你已经加载了一个 Pandoc 文档（doc），例如，通过读取一个文件
let doc = ... -- 你的 Pandoc 文档在这里

-- 将文档转换为 HTML 字符串
let htmlString = writeHtmlString def doc

-- 现在，你会像上面那样解析 `htmlString` 或根据你的需求进行处理。
```
请记住，`pandoc` 是一个更大的库，专注于多种标记格式之间的转换，因此如果你需要这些额外的能力，或者你在应用中已经在处理文档格式，就应该使用它。
